package cli

import (
	"fmt"
	"os"

	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/internal/config"
	"github.com/replit/upm/internal/util"
	"github.com/spf13/cobra"
)

func parseOutputFormat(formatStr string) outputFormat {
	switch formatStr {
	case "table":
		return outputFormatTable
	case "json":
		return outputFormatJSON
	default:
		util.Die(`Error: invalid format %#v (must be "table" or "json")`, formatStr)
		return 0
	}
}

func getVersion() string {
	return "upm development version"
}

func DoCLI() {
	if os.Getenv("UPM_NO_CHECK") == "" {
		backends.CheckAll()
	}

	var language string
	var formatStr string
	var guess bool
	var forceLock bool
	var forceInstall bool
	var all bool

	cobra.EnableCommandSorting = false

	rootCmd := &cobra.Command{
		Use:     "upm",
		Version: getVersion(),
	}
	rootCmd.SetVersionTemplate(`{{.Version}}` + "\n")
	// Not sorting the root command options because none of the
	// documented ways to disable sorting work for it (the root
	// command itself has the options sorted correctly, but they
	// are alphabetized in the help strings for subcommands).
	rootCmd.PersistentFlags().StringVarP(
		&language, "lang", "l", "", "specify project language(s) manually",
	)
	rootCmd.PersistentFlags().BoolVarP(
		&config.Quiet, "quiet", "q", false, "don't show what commands are being run",
	)
	rootCmd.PersistentFlags().BoolP(
		"help", "h", false, "display command-line usage",
	)
	rootCmd.PersistentFlags().BoolP(
		"version", "v", false, "display command version",
	)

	cmdWhichLanguage := &cobra.Command{
		Use:   "which-language",
		Short: "Query language autodetection",
		Long:  "Ask which language your project is autodetected as",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runWhichLanguage(language)
		},
	}
	rootCmd.AddCommand(cmdWhichLanguage)

	cmdListLanguages := &cobra.Command{
		Use:   "list-languages",
		Short: "List supported languages",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runListLanguages()
		},
	}
	rootCmd.AddCommand(cmdListLanguages)

	cmdSearch := &cobra.Command{
		Use:   "search QUERY...",
		Short: "Search for packages online",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			queries := args
			outputFormat := parseOutputFormat(formatStr)
			runSearch(language, queries, outputFormat)
		},
	}
	cmdSearch.Flags().SortFlags = false
	cmdSearch.Flags().StringVarP(
		&formatStr, "format", "f", "table", `output format ("table" or "json")`,
	)
	rootCmd.AddCommand(cmdSearch)

	var cmdInfo *cobra.Command
	cmdInfo = &cobra.Command{
		Aliases: []string{"show"},
		Use:     "info PACKAGE",
		Short:   "Show package information from online registry",
		Args:    cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			pkg := args[0]
			outputFormat := parseOutputFormat(formatStr)
			runInfo(language, pkg, outputFormat)
		},
	}
	cmdInfo.Flags().SortFlags = false
	cmdInfo.Flags().StringVarP(
		&formatStr, "format", "f", "table", `output format ("table" or "json")`,
	)
	rootCmd.AddCommand(cmdInfo)

	cmdAdd := &cobra.Command{
		Use:   `add "PACKAGE[ SPEC]"...`,
		Short: "Add packages to the specfile",
		Run: func(cmd *cobra.Command, args []string) {
			pkgSpecStrs := args
			runAdd(language, pkgSpecStrs, guess, forceLock, forceInstall)
		},
	}
	cmdAdd.Flags().SortFlags = false
	cmdAdd.Flags().BoolVarP(
		&guess, "guess", "g", false, "guess additional packages to add",
	)
	cmdAdd.Flags().BoolVarP(
		&forceLock, "force-lock", "f", false, "rewrite lockfile even if up to date",
	)
	cmdAdd.Flags().BoolVarP(
		&forceInstall, "force-install", "F", false, "reinstall packages even if up to date",
	)
	cmdAdd.Flags().BoolVarP(
		&config.Global, "global", "G", false, "install packages globally",
	)
	rootCmd.AddCommand(cmdAdd)

	cmdRemove := &cobra.Command{
		Use:   "remove PACKAGE...",
		Short: "Remove packages from the specfile",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			pkgs := args
			runRemove(language, pkgs, forceLock, forceInstall)
		},
	}
	cmdRemove.Flags().SortFlags = false
	cmdRemove.Flags().BoolVarP(
		&forceLock, "force-lock", "f", false, "rewrite lockfile even if up to date",
	)
	cmdRemove.Flags().BoolVarP(
		&forceInstall, "force-install", "F", false, "reinstall packages even if up to date",
	)
	cmdRemove.Flags().BoolVarP(
		&config.Global, "global", "G", false, "install packages globally",
	)
	rootCmd.AddCommand(cmdRemove)

	updateAliases := []string{"update", "upgrade"}
	cmdLock := &cobra.Command{
		Aliases: updateAliases,
		Use:     "lock",
		Short:   "Generate the lockfile from the specfile",
		Args:    cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			for _, updateAlias := range updateAliases {
				if cmd.CalledAs() == updateAlias {
					forceLock = true
				}
			}
			runLock(language, forceLock, forceInstall)
		},
	}
	cmdLock.Flags().SortFlags = false
	cmdLock.Flags().BoolVarP(
		&forceLock, "force-lock", "f", false, "rewrite lockfile even if up to date",
	)
	cmdLock.Flags().BoolVarP(
		&forceInstall, "force-install", "F", false, "reinstall packages even if up to date",
	)
	cmdLock.Flags().BoolVarP(
		&config.Global, "global", "G", false, "install packages globally",
	)
	rootCmd.AddCommand(cmdLock)

	cmdInstall := &cobra.Command{
		Use:   "install",
		Short: "Install packages from the lockfile",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runInstall(language, forceInstall)
		},
	}
	cmdInstall.Flags().SortFlags = false
	cmdInstall.Flags().BoolVarP(
		&forceInstall, "force", "F", false, "reinstall packages even if up to date",
	)
	cmdInstall.Flags().BoolVarP(
		&config.Global, "global", "G", false, "install packages globally",
	)
	rootCmd.AddCommand(cmdInstall)

	cmdList := &cobra.Command{
		Use:   "list",
		Short: "List packages from the specfile (or lockfile)",
		Long:  "List packages from the specfile",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			outputFormat := parseOutputFormat(formatStr)
			runList(language, all, outputFormat)
		},
	}
	cmdInstall.Flags().SortFlags = false
	cmdList.Flags().BoolVarP(
		&all, "all", "a", false, "list packages from the lockfile instead",
	)
	cmdList.Flags().StringVarP(
		&formatStr, "format", "f", "table", `output format ("table" or "json")`,
	)
	rootCmd.AddCommand(cmdList)

	cmdGuess := &cobra.Command{
		Use:   "guess",
		Short: "Guess what packages are needed by your project",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runGuess(language, all)
		},
	}
	cmdGuess.Flags().SortFlags = false
	cmdGuess.Flags().BoolVarP(
		&all, "all", "a", false, "list even packages already in the specfile",
	)
	rootCmd.AddCommand(cmdGuess)

	specialArgs := map[string](func()){}
	for _, helpFlag := range []string{"-help", "-?"} {
		specialArgs[helpFlag] = func() {
			rootCmd.Usage()
			os.Exit(0)
		}
	}
	for _, versionFlag := range []string{"-version", "-V"} {
		specialArgs[versionFlag] = func() {
			fmt.Println(getVersion())
			os.Exit(0)
		}
	}

	if len(os.Args) >= 2 {
		fn, ok := specialArgs[os.Args[1]]
		if ok {
			fn()
		}
	}

	rootCmd.Execute()
}
