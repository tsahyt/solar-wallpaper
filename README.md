# solar-wallpaper

A simple tool to generate wallpaper XML files for GNOME which respect solar
position at your location.

## Example

`solar-wallpaper` works using an input file, which defines the wallpaper to be
generated. Below you find an example. The field names are self-explanatory. A
stub file can be found in `res/example.toml`.

```
out.path = "/tmp/example.xml"

[img]
sunrise = "/path/to/sunrise.jpg"
noon = "/path/to/noon.jpg"
sunset = "/path/to/sunset.jpg"
evening = "/path/to/evening.jpg"
midnight = "/path/to/midnight.jpg"

[loc]
latitude = 50.0
longitude = 25.0
```

Given this example file, you can run the generator using
```
solar-wallpaper generate -i path/to/file.toml
```

If you want to immediately apply the generated wallpaper, pass the `-a` flag.

## systemd Timers

The program is designed to be used with systemd
[timers](https://www.freedesktop.org/software/systemd/man/systemd.timer.html)
or some comparable system. You can find stubs for the required unit files in
`res/solar-wallpaper.{service,timer}`.
