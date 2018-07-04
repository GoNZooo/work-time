# work-time

A library for extracting work hours from an ad-hoc work hour reporting format in
(for example) Slack.

## Reason for existing

We had already established an ad-hoc format for reporting work hours and tasks
and parsing these manually was getting tedious. Hence this library.

## Example entry

An entire entry will look as follows:

```text
Rickard Andersson [11:16 PM]
[28.02] 8.5h
worked on missile guidance system
cleaned up parsing code
```

The first part is represented by a `MessageLine` and what follows is one
`Workday` in this example.

An entry can also look as follows:

```text
steve [9:42 PM]
[01.03] 9h
fixed session management

Rickard Andersson [11:16 PM]
[28.02] 8.5h
worked on missile guidance system
cleaned up parsing code
[01.03] 6h
fixed critical bug in missile guidance system
removed half of parsing code
```

In this example we have several `Workday` in one `WorkTime`.

## Example report functionality

```haskell
> hoursFromText testData
Right [("Rickard Andersson",14.5),("steve",9.0)]
```

In the case of a `Left` being returned this will contain a more or less
informative error message from the parsing stage of the process.
