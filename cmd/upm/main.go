// Package main implements the UPM binary. It is the only
// public-facing entry point to UPM, since UPM's Go packages are all
// internal.
package main

import "github.com/replit/upm/internal/cli"

// Main entry point for the UPM binary.
func main() {
	cli.DoCLI()
}
