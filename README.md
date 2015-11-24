# ical

iCalendar parser.

Also usable as a command-line tool called `ical-org` to generate
org-mode agenda files.

## Usage

    $ ical-org
    ical-org: expected <input.ics> <output.org> [--base=YYYY-MM-DD (default: 1970-01-01)]

## Install

    $ stack install ical

## Dev

    $ git clone https://github.com/chrisdone/ical.git
    $ cd ical
    $ stack build
    $ stack exec -- ical-org input.ics output.org

## Typical use

I've setup on my server a crontab that will run this command every 5
minutes.

Here is my crontab entry:

    */5 * * * * sh /home/chris/gc-update

Here is my `gc-update` script:

``` sh
mkdir -p /tmp/ical
cd /tmp/ical

wget https://calendar.google.com/calendar/ical/chrisdone%40gmail.com/private-<my code here>/basic.ics
/home/chris/.local/bin/ical-org basic.ics basic.org --base=2015-06-01

cp basic.org /home/chris/Dropbox/Org/gc.org

rm -r /tmp/ical
```

My org-mode files are stored in Dropbox, so this update is replicated
to my Dropbox which then my desktop computer will download only if
there is any change.

Open the file e.g. `gc.org` into an Emacs buffer and enable `M-x
auto-revert-mode`.

## Example output

``` org
* DONE Cafe Bookique
  SCHEDULED: <2015-08-26>
  - State "DONE"       from "TODO"       [2015-08-26]

* DONE Lez Muvi at astra
  SCHEDULED: <2015-08-31>
  - State "DONE"       from "TODO"       [2015-08-31]

* DONE Meet Giulia at p. Italia
  SCHEDULED: <2015-09-01>
  - State "DONE"       from "TODO"       [2015-09-01]

* DONE Ping Dave on content
  SCHEDULED: <2015-09-03>
  - State "DONE"       from "TODO"       [2015-09-03]

* DONE Espressioni
  SCHEDULED: <2015-09-03>
  - State "DONE"       from "TODO"       [2015-09-03]

* DONE Stay at Hotel Glam Milano
  SCHEDULED: <2015-09-04>
  - State "DONE"       from "TODO"       [2015-09-06]

* DONE Go get a sheet for toga. And maybe a sword
  SCHEDULED: <2015-09-04>
  - State "DONE"       from "TODO"       [2015-09-04]

* DONE Get train to Milan (leaves at 17:43)
  SCHEDULED: <2015-09-04>
  - State "DONE"       from "TODO"       [2015-09-04]

* DONE The Sword live in Milan
  SCHEDULED: <2015-09-04>
  - State "DONE"       from "TODO"       [2015-09-04]

```

## Known issues

I can't be bothered to resolve time zones yet. There is some work
involved in reading them in, involving daylight savings time. For now
everything is assumed to be UTC, but given that the main use-case,
outputting org-mode files, only outputs dates, this is not a huge
problem.
