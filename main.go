package main

import (
	"fmt"
	"github.com/spf13/cobra"
	"os"
)

type InfoFormat int

const (
	InfoFormatTable InfoFormat = iota
	InfoFormatJSON
)

func runWhichLanguage() {
	fmt.Println("upm which-language")
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runListLanguages() {
	fmt.Println("upm list-languages")
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runSearch(language string, queries []string) {
	fmt.Printf("upm search %#v\n", queries)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runInfo(language string, pkg string, format InfoFormat) {
	fmt.Printf("upm info %#v --format=%#v\n", pkg, format)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runAdd(language string, pkgsWithSpecs []string, guess bool) {
	fmt.Printf("upm add %#v --guess=%#v\n", pkgsWithSpecs, guess)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runRemove(language string, pkgs []string) {
	fmt.Printf("upm remove %#v\n", pkgs)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runLock(language string, force bool) {
	fmt.Printf("upm lock --force=%#v\n", force)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runInstall(language string, force bool) {
	fmt.Printf("upm install --force=%#v\n", force)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runList(language string, all bool) {
	fmt.Printf("upm list --all=%#v\n", all)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func runGuess(language string, all bool) {
	fmt.Printf("upm guess --all=%#v\n", all)
	fmt.Println("not implemented yet")
	os.Exit(1)
}

func getVersion() string {
	return "upm development version"
}

func main() {
	var language string
	var formatStr string
	var guess bool
	var force bool
	var all bool

	rootCmd := &cobra.Command{
		Use: "upm",
		Version: getVersion(),
	}
	rootCmd.SetVersionTemplate(`{{.Version}}` + "\n")
	rootCmd.PersistentFlags().BoolP(
		"help", "h", false, "display command-line usage",
	)
	rootCmd.PersistentFlags().BoolP(
		"version", "v", false, "display command version",
	)
	rootCmd.PersistentFlags().StringVarP(
		&language, "lang", "l", "", "specify project language(s) manually",
	)

	cmdWhichLanguage := &cobra.Command{
		Use:   "which-language",
		Short: "Query language autodetection",
		Long:  "Ask which language your project is autodetected as",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runWhichLanguage()
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
			runSearch(language, queries)
		},
	}
	rootCmd.AddCommand(cmdSearch)

	var cmdInfo *cobra.Command
	cmdInfo = &cobra.Command{
		Aliases: []string{"show"},
		Use:     "info PACKAGE",
		Short:   "Show package information from online registry",
		Args:    cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			var format InfoFormat
			switch formatStr {
			case "table":
				format = InfoFormatTable
			case "json":
				format = InfoFormatJSON
			default:
				fmt.Fprintf(os.Stderr, `Error: invalid format %#v (must be "table" or "json")`, formatStr)
				fmt.Fprintln(os.Stderr)
				os.Exit(1)
			}
			pkg := args[0]
			runInfo(language, pkg, format)
		},
	}
	cmdInfo.PersistentFlags().StringVarP(
		&formatStr, "format", "f", "table", `output format ("table" or "json")`,
	)
	rootCmd.AddCommand(cmdInfo)

	cmdAdd := &cobra.Command{
		Use:   `add "PACKAGE[ SPEC]"...`,
		Short: "Add packages to the specfile",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			pkgsWithSpecs := args
			runAdd(language, pkgsWithSpecs, guess)
		},
	}
	cmdAdd.PersistentFlags().BoolVarP(
		&guess, "guess", "g", false, "guess additional packages to add",
	)
	rootCmd.AddCommand(cmdAdd)

	cmdRemove := &cobra.Command{
		Use:   "remove PACKAGE...",
		Short: "Remove packages from the specfile",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			pkgs := args
			runRemove(language, pkgs)
		},
	}
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
					force = true
				}
			}
			runLock(language, force)
		},
	}
	cmdLock.PersistentFlags().BoolVarP(
		&force, "force", "f", false, "rewrite lockfile even if up to date",
	)
	rootCmd.AddCommand(cmdLock)

	cmdInstall := &cobra.Command{
		Use:   "install",
		Short: "Install packages from the lockfile",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runInstall(language, force)
		},
	}
	cmdInstall.PersistentFlags().BoolVarP(
		&force, "force", "f", false, "reinstall packages even if up to date",
	)
	rootCmd.AddCommand(cmdInstall)

	cmdList := &cobra.Command{
		Use:   "list",
		Short: "List packages from the specfile (or lockfile)",
		Long:  "List packages from the specfile",
		Args:  cobra.NoArgs,
		Run: func(cmd *cobra.Command, args []string) {
			runList(language, all)
		},
	}
	cmdList.PersistentFlags().BoolVarP(
		&all, "all", "a", false, "list packages from the lockfile instead",
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
	cmdGuess.PersistentFlags().BoolVarP(
		&all, "all", "a", false, "list even packages already in the specfile",
	)
	rootCmd.AddCommand(cmdGuess)

	specialArgs := map[string](func ()){}
	for _, helpFlag := range []string{"-help", "-?"} {
		specialArgs[helpFlag] = func () {
			rootCmd.Usage()
			os.Exit(0)
		}
	}
	for _, versionFlag := range []string{"-version", "-V"} {
		specialArgs[versionFlag] = func () {
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
