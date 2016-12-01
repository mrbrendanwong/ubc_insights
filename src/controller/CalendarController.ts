var fs = require('fs');
var readline = require('readline');
var google = require('googleapis');
var googleAuth = require('google-auth-library');

// If modifying these scopes, delete your previously saved credentials
// at ~/.credentials/calendar-nodejs-quickstart.json
var SCOPES = ['https://www.googleapis.com/auth/calendar'];
var TOKEN_DIR = (process.env.HOME || process.env.HOMEPATH ||
    process.env.USERPROFILE) + '/.credentials/';
var TOKEN_PATH = TOKEN_DIR + 'calendar-nodejs-quickstart.json';
var scheduleObject: any;

// Load client secrets from a local file.
fs.readFile('client_secret.json', function processClientSecrets(err: any, content: any) {
    if (err) {
        console.log('Error loading client secret file: ' + err);
        return;
    }
    // Authorize a client with the loaded credentials, then call the
    // Google Calendar API.
    authorize(JSON.parse(content), scheduleEvents);
});

// Setter method
function setScheduleObject(inputSchedule: any) {
    scheduleObject = inputSchedule;
}

/**
 * Create an OAuth2 client with the given credentials, and then execute the
 * given callback function.
 *
 * @param {Object} credentials The authorization client credentials.
 * @param {function} callback The callback to call with the authorized client.
 */
function authorize(credentials: any, callback: any) {
    var clientSecret = credentials.installed.client_secret;
    var clientId = credentials.installed.client_id;
    var redirectUrl = credentials.installed.redirect_uris[0];
    var auth = new googleAuth();
    var oauth2Client = new auth.OAuth2(clientId, clientSecret, redirectUrl);

    // Check if we have previously stored a token.
    fs.readFile(TOKEN_PATH, function(err: any, token: any) {
        if (err) {
            getNewToken(oauth2Client, callback);
        } else {
            oauth2Client.credentials = JSON.parse(token);
            callback(oauth2Client);
        }
    });
}

/**
 * Get and store new token after prompting for user authorization, and then
 * execute the given callback with the authorized OAuth2 client.
 *
 * @param {google.auth.OAuth2} oauth2Client The OAuth2 client to get token for.
 * @param {getEventsCallback} callback The callback to call with the authorized
 *     client.
 */
function getNewToken(oauth2Client: any, callback: any) {
    var authUrl = oauth2Client.generateAuthUrl({
        access_type: 'offline',
        scope: SCOPES
    });
    console.log('Authorize this app by visiting this url: ', authUrl);
    var rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    rl.question('Enter the code from that page here: ', function(code: any) {
        rl.close();
        oauth2Client.getToken(code, function(err: any, token: any) {
            if (err) {
                console.log('Error while trying to retrieve access token', err);
                return;
            }
            oauth2Client.credentials = token;
            storeToken(token);
            callback(oauth2Client);
        });
    });
}

/**
 * Store token to disk be used in later program executions.
 *
 * @param {Object} token The token to store to disk.
 */
function storeToken(token: any) {
    try {
        fs.mkdirSync(TOKEN_DIR);
    } catch (err) {
        if (err.code != 'EEXIST') {
            throw err;
        }
    }
    fs.writeFile(TOKEN_PATH, JSON.stringify(token));
    console.log('Token stored to ' + TOKEN_PATH);
}

function createEvent(auth: any, daySchedule: any, currentBuilding:any, calendar:any, eventDay: any, recurUntil: any) {
    for (var j = 0; j < daySchedule.length; j++) {
        let timeSlot = Object.keys(daySchedule[j])[0];
        let timeSlotContents = daySchedule[j][timeSlot];
        if (timeSlotContents != "") {
            let startEndTime: any = timeSlot.split("-");

            let summary: any = timeSlotContents; // This is course
            let location: any = currentBuilding;
            let startTime: any = startEndTime[0]; // Start time
            let endTime: any = startEndTime[1]; // End time

            let event: any = {
                'summary': summary,
                'location': location,
                'description': 'A course to be scheduled at UBC',
                'start': {
                    'dateTime': eventDay + startTime + ':00-07:00',
                    'timeZone': 'America/Los_Angeles',
                },
                'end': {
                    'dateTime': eventDay + endTime + ':00-07:00',
                    'timeZone': 'America/Los_Angeles',
                },
                "recurrence": [
                    "RRULE:FREQ=WEEKLY;UNTIL=2016" + recurUntil + "T235900Z",
                ],
            };

            calendar.events.insert({
                auth: auth,
                calendarId: 'primary',
                resource: event,
            }, function(err: any, event: any, response: any) {
                if (err) {
                    console.log('There was an error contacting the Calendar service: ' + err);
                    return;
                }
                console.log('Event created: %s', event.htmlLink);
            });
        }
    }
}

function createCalendar() {}

// '2016-09-05T'
function scheduleEvents(auth: any) {
    var calendar = google.calendar('v3');
    var scheduleObj: any = [
        [
            {"courses_uuid":41,"courses_dept":"cpsc","courses_id":"301","courses_pass":200,"courses_fail":0},
            {"courses_uuid":43,"courses_dept":"cpsc","courses_id":"301","courses_pass":200,"courses_fail":0},
            {"courses_uuid":44,"courses_dept":"cpsc","courses_id":"301","courses_pass":200,"courses_fail":0},
            {"courses_uuid":45,"courses_dept":"cpsc","courses_id":"301","courses_pass":200,"courses_fail":0},
            {"courses_uuid":42,"courses_dept":"cpsc","courses_id":"301","courses_pass":200,"courses_fail":0}
        ],
        {"DMP 110":
            {"mwf":[{"00:00-01:00":"CPSC 301 16 - Section Size: 100"},
                {"01:00-02:00":"CPSC 301 17 - Section Size: 100"},
                {"02:00-03:00":"CPSC 301 18 - Section Size: 100"},
                {"03:00-04:00":"CPSC 301 19 - Section Size: 100"},
                {"04:00-05:00":"CPSC 301 20 - Section Size: 100"},
                {"05:00-06:00":"CPSC 301 21 - Section Size: 100"},
                {"06:00-07:00":"CPSC 301 22 - Section Size: 100"},
                {"07:00-08:00":"CPSC 301 23 - Section Size: 100"},
                {"08:00-09:00":"CPSC 301 1 - Section Size: 100"},
                {"09:00-10:00":"CPSC 301 2 - Section Size: 100"},
                {"10:00-11:00":"CPSC 301 3 - Section Size: 100"},
                {"11:00-12:00":"CPSC 301 4 - Section Size: 100"},
                {"12:00-13:00":"CPSC 301 5 - Section Size: 100"},
                {"13:00-14:00":"CPSC 301 6 - Section Size: 100"},
                {"14:00-15:00":"CPSC 301 7 - Section Size: 100"},
                {"15:00-16:00":"CPSC 301 8 - Section Size: 100"},
                {"16:00-17:00":"CPSC 301 9 - Section Size: 100"},
                {"17:00-18:00":"CPSC 301 29 - Section Size: 100"},
                {"18:00-19:00":"CPSC 301 30 - Section Size: 100"},
                {"19:00-20:00":""},
                {"20:00-21:00":""},
                {"21:00-22:00":""},
                {"22:00-23:00":""},
                {"23:00-23:59":""}],
                "tt":
                    [{"00:30-02:00":"CPSC 301 24 - Section Size: 100"},
                        {"02:00-03:30":"CPSC 301 25 - Section Size: 100"},
                        {"03:30-05:00":"CPSC 301 26 - Section Size: 100"},
                        {"05:00-06:30":"CPSC 301 27 - Section Size: 100"},
                        {"06:30-08:00":"CPSC 301 28 - Section Size: 100"},
                        {"08:00-09:30":"CPSC 301 10 - Section Size: 100"},
                        {"09:30-11:00":"CPSC 301 11 - Section Size: 100"},
                        {"11:00-12:30":"CPSC 301 12 - Section Size: 100"},
                        {"12:30-14:00":"CPSC 301 13 - Section Size: 100"},
                        {"14:00-15:30":"CPSC 301 14 - Section Size: 100"},
                        {"15:30-17:00":"CPSC 301 15 - Section Size: 100"},
                        {"17:00-18:30":""},
                        {"18:30-20:00":""},
                        {"20:00-21:30":""},
                        {"21:30-23:00":""},
                        {"23:00-23:59":""}],
                "seats":150,
                "quality":50},

            "DMP 310":
                {"mwf":
                    [{"00:00-01:00":""},
                        {"01:00-02:00":""},
                        {"02:00-03:00":""},
                        {"03:00-04:00":""},
                        {"04:00-05:00":""},
                        {"05:00-06:00":""},
                        {"06:00-07:00":""},
                        {"07:00-08:00":""},
                        {"08:00-09:00":""},
                        {"09:00-10:00":""},
                        {"10:00-11:00":""},
                        {"11:00-12:00":""},
                        {"12:00-13:00":""},
                        {"13:00-14:00":""},
                        {"14:00-15:00":""},
                        {"15:00-16:00":""},
                        {"16:00-17:00":""},
                        {"17:00-18:00":""},
                        {"18:00-19:00":""},
                        {"19:00-20:00":"CPSC 301 100 - Section Size: 50"},
                        {"20:00-21:00":"CPSC 301 101 - Section Size: 50"},
                        {"21:00-22:00":"CPSC 301 102 - Section Size: 50"},
                        {"22:00-23:00":"CPSC 301 103 - Section Size: 50"},
                        {"23:00-23:59":"CPSC 301 104 - Section Size: 50"}],
                    "tt":
                        [{"00:30-02:00":""},
                            {"02:00-03:30":""},
                            {"03:30-05:00":""},
                            {"05:00-06:30":""},
                            {"06:30-08:00":""},
                            {"08:00-09:30":""},
                            {"09:30-11:00":""},
                            {"11:00-12:30":""},
                            {"12:30-14:00":""},
                            {"14:00-15:30":""},
                            {"15:30-17:00":""},
                            {"17:00-18:30":""},
                            {"18:30-20:00":""},
                            {"20:00-21:30":""},
                            {"21:30-23:00":""},
                            {"23:00-23:59":""}],
                    "seats":50,
                    "quality":0}}
    ];
    //var scheduleObj = scheduleObject;
    var fullSchedule: any = scheduleObj[1];
    var scheduleKeys: any = Object.keys(fullSchedule);


    for (var i = 0; i < scheduleKeys.length; i++) {
        var currentBuilding: string = scheduleKeys[i];
        let mwfSchedule = fullSchedule[currentBuilding]['mwf'];
        let ttSchedule = fullSchedule[currentBuilding]['tt'];

        // MONDAY
        createEvent(auth, mwfSchedule, currentBuilding, calendar, '2016-09-05T', "1203");

        // TUESDAY
        setTimeout(function(){createEvent(auth, ttSchedule, currentBuilding, calendar, '2016-09-06T', "1203")}, 4000);

        // WEDNESDAY
        setTimeout(function(){createEvent(auth, mwfSchedule, currentBuilding, calendar, '2016-09-07T', "1203")}, 8000);

        // THURSDAY
        setTimeout(function(){createEvent(auth, ttSchedule, currentBuilding, calendar, '2016-09-08T', "1203")}, 12000);

        // FRIDAY
        setTimeout(function(){createEvent(auth, mwfSchedule, currentBuilding, calendar, '2016-09-09T', "1203")}, 16000);
        console.log("COMPLETE");
    }
}