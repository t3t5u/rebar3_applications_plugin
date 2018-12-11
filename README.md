[![Hex.pm Version](https://img.shields.io/hexpm/v/rebar3_applications_plugin.svg)](https://hex.pm/packages/rebar3_applications_plugin)
[![Hex.pm License](https://img.shields.io/hexpm/l/rebar3_applications_plugin.svg)](LICENSE)

# rebar3_applications_plugin

A rebar3 plugin for automatically generate the applications in the application resource file.

## Usage

Add to your top level `rebar.config`:

```erlang
{plugins, [rebar3_applications_plugin]}.
{provider_hooks, [{post, [{compile, applications}]}]}.
```

## License
[MIT](LICENSE)
