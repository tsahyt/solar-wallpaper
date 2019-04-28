# solar-wallpaper

A simple tool to generate wallpaper XML files for GNOME which respect solar
position at your location.

## Example

`solar-wallpaper` works using an input file, which defines the wallpaper to be
generated. Below you find an example. The field names are self-explanatory, a
reference can be found below.. A stub file can be found in `res/example.toml`.

```toml
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

## Input Reference
+ `out.path`, Path where output file is to be stored
+ `bias`, *optional*, a bias value to speed up or slow down the transition into
  the "evening" image. The lower the number, the faster sunset will transition
  into evening, but the slower evening will transition into midnight.
+ `img.sunrise`, Image to use at sunrise
+ `img.noon`, Image to use at solar noon. Note that this can deviate from 12:00.
+ `img.sunset`, Image to use at sunset
+ `img.evening`, Image to use in the evening. Evening is placed inbetween
  sunset and midnight, and depends on the `bias`.
+ `img.midnight`, Image to use at solar midnight. Note that this can deviate from 00:00.
+ `loc.latitude`, The latitude of your location.
+ `loc.longitude`, The longitude of your location.
