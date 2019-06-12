package internal

import (
	"fmt"
	"github.com/spf13/cobra"
	"os"
	"strings"
)

type outputFormat int

const (
	outputFormatTable outputFormat = iota
	outputFormatJSON
)

func parseOutputFormat(formatStr string) outputFormat {
	switch formatStr {
	case "table":
		return outputFormatTable
	case "json":
		return outputFormatJSON
	default:
		die(`Error: invalid format %#v (must be "table" or "json")`, formatStr)
		return 0
	}
}

func runWhichLanguage() {
	backend := getBackend("")
	fmt.Println(backend.name)
}

func runListLanguages() {
	for _, backendName := range getBackendNames() {
		fmt.Println(backendName)
	}
}

func runSearch(language string, queries []string, outputFormat outputFormat) {
	results := getBackend(language).search(queries)
	fmt.Printf("output %#v in format %#v\n", results, outputFormat)
	notImplemented()
}

func runInfo(language string, pkg string, outputFormat outputFormat) {
	info := getBackend(language).info(pkg)
	fmt.Printf("output %#v in format %#v\n", info, outputFormat)
	notImplemented()
}

func runAdd(language string, pkgSpecStrs []string, guess bool) {
	pkgSpecs := []pkgSpec{}
	for _, pkgSpecStr := range pkgSpecStrs {
		fields := strings.Fields(pkgSpecStr)
		if !(len(fields) >= 1 && len(fields) <= 2) {
			fmt.Fprintf(os.Stderr, "invalid package/spec: %#v\n", pkgSpecStr)
			os.Exit(1)
		}
		pkg := fields[0]
		var spec string
		if len(fields) >= 2 {
			spec = fields[1]
		}
		pkgSpecs = append(pkgSpecs, pkgSpec{pkg, spec})
	}

	backend := getBackend(language)
	if guess {
		for _, guessedPkg := range backend.guess() {
			alreadyAdded := false
			for _, givenSpec := range pkgSpecs {
				if givenSpec.pkg == guessedPkg {
					alreadyAdded = true
					break
				}
			}
			if !alreadyAdded {
				pkgSpecs = append(pkgSpecs, pkgSpec{pkg: guessedPkg})
			}
		}
	}

	existingSpecs := backend.listSpecfile()
	filteredSpecs := []pkgSpec{}
	for _, spec := range pkgSpecs {
		alreadyExists := false
		for _, existingSpec := range existingSpecs {
			if (existingSpec.pkg == spec.pkg) {
				alreadyExists = true
				break
			}
		}
		if (!alreadyExists) {
			filteredSpecs = append(filteredSpecs, spec)
		}
	}

	backend.add(filteredSpecs)
}

func runRemove(language string, pkgs []string) {
	getBackend(language).remove(pkgs)
}

func runLock(language string, force bool) {
	notImplemented()
}

func runInstall(language string, force bool) {
	notImplemented()
}

func runList(language string, all bool, outputFormat outputFormat) {
	backend := getBackend(language)
	if all {
		results := backend.listLockfile()
		fmt.Printf("output %#v in format %#v\n", results, outputFormat)
		notImplemented()
	} else {
		results := backend.listSpecfile()
		fmt.Printf("output %#v in format %#v\n", results, outputFormat)
		notImplemented()
	}
}

func runGuess(language string, all bool) {
	backend := getBackend(language)
	guessedPkgs := backend.guess()
	if (!all) {
		existingSpecs := backend.listSpecfile()
		filteredGuesses := []string{}
		for _, guessedPkg := range guessedPkgs {
			alreadyAdded := false
			for _, existingSpec := range existingSpecs {
				if existingSpec.pkg == guessedPkg {
					alreadyAdded = true
					break
				}
			}
			if !alreadyAdded {
				filteredGuesses = append(filteredGuesses, guessedPkg)
			}
		}
		guessedPkgs = filteredGuesses
	}

	for _, pkg := range guessedPkgs {
		fmt.Println(pkg)
	}
}

func getVersion() string {
	return "upm development version"
}

func DoCLI() {
	var language string
	var formatStr string
	var guess bool
	var force bool
	var all bool

	rootCmd := &cobra.Command{
		Use:     "upm",
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
			outputFormat := parseOutputFormat(formatStr)
			runSearch(language, queries, outputFormat)
		},
	}
	cmdSearch.PersistentFlags().StringVarP(
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
	cmdInfo.PersistentFlags().StringVarP(
		&formatStr, "format", "f", "table", `output format ("table" or "json")`,
	)
	rootCmd.AddCommand(cmdInfo)

	cmdAdd := &cobra.Command{
		Use:   `add "PACKAGE[ SPEC]"...`,
		Short: "Add packages to the specfile",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			pkgSpecStrs := args
			runAdd(language, pkgSpecStrs, guess)
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
			outputFormat := parseOutputFormat(formatStr)
			runList(language, all, outputFormat)
		},
	}
	cmdList.PersistentFlags().BoolVarP(
		&all, "all", "a", false, "list packages from the lockfile instead",
	)
	cmdList.PersistentFlags().StringVarP(
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
	cmdGuess.PersistentFlags().BoolVarP(
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
