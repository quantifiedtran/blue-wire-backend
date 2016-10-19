# blue-wire-backend

A server for blue-wire, written in haskell.

## API

### POST

#### `/new`

Create a new application profile, takes a JSON representation of an `AppStats` datatype in the request body. Overwrites `lastHeartbeat` with the time the current time on the server. Returns the current UTC time of the server.

#### `/heartbeat/:application`

"heartbeat" an application profile to let the server know the app is still open. Takes an empty request body, returns a `HeartbeatResponse`:

```js
// The components of the response.
// UpcomingKick
{
    timeUntilKick: ..., // Time in seconds.
    kickWouldLast: ..., // Time in seconds.
}

// KickResponse
{
    kickEndsOn: ..., // UTC Timestamp for when the current kick ends.
}

// InfoResponse
{
    upcomingKicks: [...], // an array of UpcomingKick objects
}

// The actual data retrived.
// HeartbeatResponse
{
    Left: ...? // KickResponse, will be null/undefined if Right is defined.
    Right: ...? // InfoResponse, will be null/undefined if Left is defined.
}
```

#### `/set/:application/kicks`
