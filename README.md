# ical

iCalendar parser.

## Usage

    $ ical-org
    ical-org: expected <input.ics> <output.org> [--base=YYYY-MM-DD (default: 1970-01-01)]

## Typical use

I've setup on my server a crontab that will run this command every 5
minutes. My org-mode files are stored in dropbox, so this update is
replicated to my dropbox which then my desktop computer will download
if there is any change.
