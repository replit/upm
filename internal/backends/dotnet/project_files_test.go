package dotnet

import (
	"strings"
	"testing"

	"github.com/replit/upm/internal/api"
)

func TestReadSpec(t *testing.T) {
	spec := strings.NewReader(
		`<Project Sdk="Microsoft.NET.Sdk">
		<PropertyGroup>
			<OutputType>Exe</OutputType>
			<TargetFramework>net5.0</TargetFramework>
		</PropertyGroup>
		<ItemGroup>
			<PackageReference Include="Microsoft.Extensions.Logging" Version="5.0.0" />
			<PackageReference Include="Newtonsoft.Json" Version="13.0.1" />
			<PackageReference Include="SharpYaml" Version="1.6.6" />
		</ItemGroup>
	</Project>`)

	pkgs, err := ReadSpec(spec)
	if err != nil {
		t.Errorf("Failed to read spec with error %q", err)
	}

	for pkg, version := range map[string]string{"Microsoft.Extensions.Logging": "5.0.0", "Newtonsoft.Json": "13.0.1", "SharpYaml": "1.6.6"} {
		if pkgs[api.PkgName(pkg)] != api.PkgSpec(version) {
			t.Errorf("Wrong version %s for %s", pkgs[api.PkgName(pkg)], pkg)
		}
	}
}

func TestReadSpecWithoutDependencies(t *testing.T) {
	spec := strings.NewReader(
		`<Project Sdk="Microsoft.NET.Sdk">
		<PropertyGroup>
			<OutputType>Exe</OutputType>
			<TargetFramework>net5.0</TargetFramework>
		</PropertyGroup>
	</Project>`)

	pkgs, err := ReadSpec(spec)
	if err != nil {
		t.Errorf("Failed to read spec with error %q", err)
	}

	if len(pkgs) != 0 {
		t.Errorf("Unexpected data in spec file %q", pkgs)
	}
}

func TestReadLock(t *testing.T) {
	lock := strings.NewReader(
		`{
			"version": 1,
			"dependencies": {
			  ".NETCoreApp,Version=v5.0": {
				"Microsoft.Extensions.Logging": {
				  "type": "Direct",
				  "requested": "[5.0.0, )",
				  "resolved": "5.0.0",
				  "contentHash": "MgOwK6tPzB6YNH21wssJcw/2MKwee8b2gI7SllYfn6rvTpIrVvVS5HAjSU2vqSku1fwqRvWP0MdIi14qjd93Aw==",
				  "dependencies": {
					"Microsoft.Extensions.DependencyInjection": "5.0.0",
					"Microsoft.Extensions.DependencyInjection.Abstractions": "5.0.0",
					"Microsoft.Extensions.Logging.Abstractions": "5.0.0",
					"Microsoft.Extensions.Options": "5.0.0"
				  }
				},
				"Newtonsoft.Json": {
				  "type": "Direct",
				  "requested": "[13.0.1, )",
				  "resolved": "13.0.1",
				  "contentHash": "ppPFpBcvxdsfUonNcvITKqLl3bqxWbDCZIzDWHzjpdAHRFfZe0Dw9HmA0+za13IdyrgJwpkDTDA9fHaxOrt20A=="
				}
			  }
			}
		}`)

	pkgs, err := ReadLock(lock)

	if err != nil {
		t.Errorf("Failed to read lock with error %q", err)
	}

	for pkg, version := range map[string]string{"Microsoft.Extensions.Logging": "5.0.0", "Newtonsoft.Json": "13.0.1"} {
		if pkgs[api.PkgName(pkg)] != api.PkgVersion(version) {
			t.Errorf("Wring version %s for %s", pkgs[api.PkgName(pkg)], pkg)
		}
	}
}

func TestReadLockWithProjectDependency(t *testing.T) {
	lock := strings.NewReader(
		`{
			"version": 1,
			"dependencies": {
				".NETStandard,Version=v2.0": {
					"System.Text.Json": {
						"type": "Transitive",
						"resolved": "5.0.1",
						"contentHash": "/UM3UK1dXKl8Ybysg/21gM4S8DJgkR+yLU8JwqCVbuNqQNImelntgYFAN5QxR8sJJ1kMx//hOUdf0lltosi8cQ==",
						"dependencies": {
						"Microsoft.Bcl.AsyncInterfaces": "5.0.0",
						"System.Buffers": "4.5.1",
						"System.Memory": "4.5.4",
						"System.Numerics.Vectors": "4.5.0",
						"System.Runtime.CompilerServices.Unsafe": "5.0.0",
						"System.Text.Encodings.Web": "5.0.0",
						"System.Threading.Tasks.Extensions": "4.5.4"
						}
					},
					"xunit.v3.common": {
						"type": "Project",
						"dependencies": {
						"System.Text.Json": "5.0.1"
						}
					}
				}
		    }
		}`)

	pkgs, err := ReadLock(lock)

	if err != nil {
		t.Errorf("Failed to read lock with error %q", err)
	}

	for pkg, version := range map[string]string{"System.Text.Json": "5.0.1"} {
		if pkgs[api.PkgName(pkg)] != api.PkgVersion(version) {
			t.Errorf("Wrong version %s for %s", pkgs[api.PkgName(pkg)], pkg)
		}
	}
}
