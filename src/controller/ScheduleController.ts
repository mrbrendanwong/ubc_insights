/**
 * Created by Brendon on 2016-11-26.
 */
import Log from "../Util";
import DatasetController from '../controller/DatasetController';
import QueryController from "../controller/QueryController";

var Calendar = require('moment-calendar');

export default class ScheduleController {

    private findInvalidCourses(courses: any): any {};

    private removeInvalidCoures(courses: any, invalidCourses: any): any {};

    private buildRoomSchedule(rooms: any): any {};

    private findATime(): any {};

    public scheduleRooms(courses: any, rooms: any): any {
        //Sort rooms and courses from largest sizes to smallest
        var sortedCourses = courses.sort(); //Assume sorted
        var sortedRooms = rooms.sort(); //Assume sorted

        //List of courses that cannot fit into any rooms
        var invalidScheduling: any = [];
        var fullSchedule: any = {}

        // Make schedule for each room
        for (var y = 0; y < sortedRooms.length; y++) {
            let roomTitle: string = sortedRooms[y]["rooms_shortname"] + " " + sortedRooms[y]["rooms_id"]
            let roomSchedule: any = {
                "mwf":[
                    {"00:00-01:00":""},
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
                    {"19:00-20:00":""},
                    {"20:00-21:00":""},
                    {"21:00-22:00":""},
                    {"22:00-23:00":""},
                    {"23:00-24:00":""}],

                "tt":[
                    {"00:30-02:00":""},
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
                    {"23:00-00:30":""}]
            }

            fullSchedule[roomTitle] = roomSchedule;
        }

        //Find all invalid courses
        for (var i = 0; i < sortedCourses.length; i++) {
            // compare course sizes with room sizes
            // if course size > room size, call invalidScheduling to add to invalid scheduling list
            // remove invalid room from schedule
            // fixed array is sortedCourses = sortedCourses.splice(x, 1)
            let sectionSize: number = sortedCourses[i]["courses_pass"] + sortedCourses[i]["courses_fail"];
            let roomSize: number = sortedRooms[0]["rooms_seats"];
            if  (sectionSize > roomSize) {
                invalidScheduling.push(sortedCourses[i]);
            }
        }

        //Remove invalid courses from list
        for (var x = 0; x < invalidScheduling.length; x++) {
            let indexOfInvalid = sortedCourses.indexOf(invalidScheduling[x]);
            if (indexOfInvalid > -1)
                sortedCourses.splice(indexOfInvalid,1);
        }

        // Start trying to assign courses to room schedules
        for (var j = 0; j < sortedCourses.length; j++) {
            // Find diff between current course and all rooms (rooms_size - (courses_pass + courses_fail))
            // Store rooms and diffs in object consisting of kv pair rooms_shortname + rooms_id: roomSizeDiff
            // Only store if diff is > 0
            var currentSizeDiffs: any = [];
            for (var k = 0; k < sortedRooms.length; k++) {
                let roomTitle: string = sortedRooms[k]["rooms_shortname"] + " " + sortedRooms[k]["rooms_id"];
                let sectionSize: number = sortedCourses[j]["courses_pass"] + sortedCourses[j]["courses_fail"];
                let roomSize: number = sortedRooms[k]["rooms_seats"];
                let sizeDiff: number = roomSize - sectionSize;
                if (sizeDiff >= 0) {
                    let roomSizeDiff: any = {};
                    roomSizeDiff[roomTitle] = sizeDiff;
                    currentSizeDiffs.push(roomSizeDiff);
                }
            }

            // Current course name
            var currentCourse = sortedCourses[j]["courses_uuid"] + " " + sortedCourses[j]["courses_dept"] + " " + sortedCourses[j]["courses_id"]

            // Sort rooms by min difference (smallest first)
            var sortedCurrentSizeDiffs =  currentSizeDiffs.sort();

            // Check in schedule for room to see if there is time available between 8-5 on MWF, else check TT
            // When we find a a time, check if there is another course offering occuring at the chosen time
            // Go through entire schedule and check if current time chosen contains a course with substring courses_dept + courses_id
            var foundATime: boolean = false;
            for (var a = 0; a < sortedCurrentSizeDiffs.length; a++) { // Going through each room
                let currentRoom: string = Object.keys(sortedCurrentSizeDiffs[a])[0];
                // Go through MWF
                for (var b = 8; b < 18; b++) {
                    let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['mwf'][b])[0];
                    if (Object.values(fullSchedule[currentRoom]['mwf'][b])[0] == "") {
                        // Found empty time, check if any other room has course offering at same index (time)
                        let conflictOffering: boolean = false;
                        for (var e = 0; e < sortedRooms.length; e++){
                            let otherRoom = sortedRooms[e]["rooms_shortname"] + " " + sortedRooms[e]["rooms_id"];
                            if (Object.values(fullSchedule[otherRoom]['mwf'][b])[0] == currentCourse)
                                conflictOffering = true;
                        }
                        if (conflictOffering) {
                            continue;
                        } else {
                            foundATime = true;
                            fullSchedule[currentRoom]['mwf'][b][currentTimeSlot] = currentCourse;
                        }
                    }
                }
                // If no time in MWF, check TT
                if (!foundATime) {
                    for (var c = 5; c < 11; c++) {
                        let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['tt'][c])[0];
                        if (Object.values(fullSchedule[currentRoom]['tt'][c])[0] == "") {
                            // Found empty time, check if any other room has course offering at same index (time)
                            let conflictOffering: boolean = false;
                            for (var f = 0; f < sortedRooms.length; f++){
                                let otherRoom = sortedRooms[f]["rooms_shortname"] + " " + sortedRooms[f]["rooms_id"];
                                if (Object.values(fullSchedule[otherRoom]['tt'][c])[0] == currentCourse)
                                    conflictOffering = true;
                            }
                            if (conflictOffering) {
                                continue;
                            } else {
                                foundATime = true;
                                fullSchedule[currentRoom]['tt'][c][currentTimeSlot] = currentCourse;
                            }
                        }
                    }
                }
                if (foundATime)
                    break;
            }

            // If we can't find a best time (8-17), find a morning time (0-7)
            // a to l, b to m, c to n, e to o, f to p
            if (!foundATime) {
                for (var l = 0; l < sortedCurrentSizeDiffs.length; l++) { // Going through each room
                    let currentRoom: string = Object.keys(sortedCurrentSizeDiffs[l])[0];
                    // Go through MWF
                    for (var m = 0; m < 8; m++) {
                        let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['mwf'][m])[0];
                        if (Object.values(fullSchedule[currentRoom]['mwf'][m])[0] == "") {
                            // Found empty time, check if any other room has course offering at same index (time)
                            let conflictOffering: boolean = false;
                            for (var o = 0; o < sortedRooms.length; o++){
                                let otherRoom = sortedRooms[o]["rooms_shortname"] + " " + sortedRooms[o]["rooms_id"];
                                if (Object.values(fullSchedule[otherRoom]['mwf'][m])[0] == currentCourse)
                                    conflictOffering = true;
                            }
                            if (conflictOffering) {
                                continue;
                            } else {
                                foundATime = true;
                                fullSchedule[currentRoom]['mwf'][m][currentTimeSlot] = currentCourse;
                            }
                        }
                    }
                    // If no time in MWF, check TT
                    if (!foundATime) {
                        for (var n = 0; n < 5; n++) {
                            let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['tt'][n])[0];
                            if (Object.values(fullSchedule[currentRoom]['tt'][n])[0] == "") {
                                // Found empty time, check if any other room has course offering at same index (time)
                                let conflictOffering: boolean = false;
                                for (var p = 0; p < sortedRooms.length; p++){
                                    let otherRoom = sortedRooms[p]["rooms_shortname"] + " " + sortedRooms[p]["rooms_id"];
                                    if (Object.values(fullSchedule[otherRoom]['tt'][n])[0] == currentCourse)
                                        conflictOffering = true;
                                }
                                if (conflictOffering) {
                                    continue;
                                } else {
                                    foundATime = true;
                                    fullSchedule[currentRoom]['tt'][n][currentTimeSlot] = currentCourse;
                                }
                            }
                        }
                    }
                    if (foundATime)
                        break;
                }
            }

            // If we can't find a best time (12AM-7AM), find a night time (5PM-11PM)
            // a to q, b to r, c to s, e to t, f to u
            if (!foundATime) {
                for (var q = 0; q < sortedCurrentSizeDiffs.length; q++) { // Going through each room
                    let currentRoom: string = Object.keys(sortedCurrentSizeDiffs[q])[0];
                    // Go through MWF
                    for (var r = 8; r < 18; r++) {
                        let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['mwf'][r])[0];
                        if (Object.values(fullSchedule[currentRoom]['mwf'][r])[0] == "") {
                            // Found empty time, check if any other room has course offering at same index (time)
                            let conflictOffering: boolean = false;
                            for (var t = 0; t < sortedRooms.length; t++){
                                let otherRoom = sortedRooms[t]["rooms_shortname"] + " " + sortedRooms[t]["rooms_id"];
                                if (Object.values(fullSchedule[otherRoom]['mwf'][r])[0] == currentCourse)
                                    conflictOffering = true;
                            }
                            if (conflictOffering) {
                                continue;
                            } else {
                                foundATime = true;
                                fullSchedule[currentRoom]['mwf'][r][currentTimeSlot] = currentCourse;
                            }
                        }
                    }
                    // If no time in MWF, check TT
                    if (!foundATime) {
                        for (var s = 5; s < 11; s++) {
                            let currentTimeSlot = Object.keys(fullSchedule[currentRoom]['tt'][s])[0];
                            if (Object.values(fullSchedule[currentRoom]['tt'][s])[0] == "") {
                                // Found empty time, check if any other room has course offering at same index (time)
                                let conflictOffering: boolean = false;
                                for (var u = 0; u < sortedRooms.length; u++){
                                    let otherRoom = sortedRooms[u]["rooms_shortname"] + " " + sortedRooms[u]["rooms_id"];
                                    if (Object.values(fullSchedule[otherRoom]['tt'][s])[0] == currentCourse)
                                        conflictOffering = true;
                                }
                                if (conflictOffering) {
                                    continue;
                                } else {
                                    foundATime = true;
                                    fullSchedule[currentRoom]['tt'][s][currentTimeSlot] = currentCourse;
                                }
                            }
                        }
                    }
                    if (foundATime)
                        break;
                }
            }

            // No times at all, so we cannot schedule the course
            // Put into invalid list
            if (!foundATime)
                invalidScheduling.push(sortedCourses[j]);
        }

        return fullSchedule;
    }

}