# ical

iCalendar parser.

## Usage

    $ ical-org
    ical-org: expected <input.ics> <output.org> [--base=YYYY-MM-DD (default: 1970-01-01)]

## Typical use

I've setup on my server a crontab that will run this command every 5
minutes. My org-mode files are stored in dropbox, so this update is
replicated to my dropbox which then my desktop computer will download
if there is any change. Open the file e.g. `gc.org` into an Emacs
buffer and enable `M-x auto-revert-mode`.

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
