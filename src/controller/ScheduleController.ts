/**
 * Created by Brendon on 2016-11-26.
 */
export default class ScheduleController {


    public launchCommand():any {
        var exec = require('child_process').exec;
        var cmd = 'ls';

        exec(cmd, function(error:any, stdout:any, stderr:any) {
            console.log(stdout);
            // command output is in stdout
        });
    };
    /**
     * Build a schedule for every single room
     * Will return in the form of
     * {"room_shortname" + "rooms_number" : room_schedule, ...}
     * @param rooms
     * @returns {any}
     */
    private buildRoomSchedule(rooms: any): any {
        var fullSchedule: any = {};
        for (var i = 0; i < rooms.length; i++) {
            let roomTitle: string = rooms[i]["rooms_shortname"] + " " + rooms[i]["rooms_number"];
            fullSchedule[roomTitle] = {
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
                    {"23:00-00:30":""}],
                "seats": rooms[i]["rooms_seats"],
                "quality": 100
            }
        }
        return fullSchedule;
    }

    /**
     * Compare course section size with room seats
     * If course size is bigger than room size, add to list invalidScheduling
     * @param courses
     * @param rooms
     * @param invalidScheduling
     * @returns {any[]}
     */
    private findInvalidCourses(courses: any[], rooms: any[], invalidScheduling:any[]): any {
        for (var i = 0; i < courses.length; i++) {
            let sectionSize: number = courses[i]["courses_pass"] + courses[i]["courses_fail"];
            let roomSize: number = rooms[0]["rooms_seats"];
            if  (sectionSize > roomSize) {
                invalidScheduling.push(courses[i]);
            }
        }
        return invalidScheduling;
    }

    /**
     * Remove invalid courses from list of courses to schedule
     * @param courses
     * @param invalidScheduling
     * @returns {any[]}
     */
    private removeInvalidCoures(courses: any[], invalidScheduling: any[]): any {
        for (var i = 0; i < invalidScheduling.length; i++) {
            let indexOfInvalid = courses.indexOf(invalidScheduling[i]);
            if (indexOfInvalid > -1)
                courses.splice(indexOfInvalid,1);
        }
        return courses;
    }

    /**
     * Return all size differences between a course and all rooms
     * Returns in form of [{rooms_shortname + rooms_number: roomSizeDiff}, ...]
     * Only store if diff is > 0
     * @param course
     * @param rooms
     * @returns {any}
     */
    private getRoomSizeDiffs(course: any, rooms:any): any {
        var sizeDiffs: any = [];
        for (var i = 0; i < rooms.length; i++) {
            let roomTitle: string = rooms[i]["rooms_shortname"] + " " + rooms[i]["rooms_number"];
            let sectionSize: number = course["courses_pass"] + course["courses_fail"];
            let roomSize: number = rooms[i]["rooms_seats"];
            let sizeDiff: number = roomSize - sectionSize;
            if (sizeDiff >= 0) {
                let roomSizeDiff: any = [roomTitle, sizeDiff];
                sizeDiffs.push(roomSizeDiff);
            }
        }
        return sizeDiffs
    }

    /**
     * Inserts a course into a valid time slot in a given full schedule
     * @param courseShortName
     * @param courseName
     * @param rooms
     * @param room
     * @param fullSchedule
     * @param scheduleDay
     * @param startTime
     * @param endTime
     * @param foundATime
     * @returns {(boolean|any)[]}
     */
    private findATime(courseShortName: any, courseName:any, rooms:any, room:any, fullSchedule:any, scheduleDay:string, startTime:number, endTime:number, foundATime:boolean): any {
        for (var b = startTime; b < endTime; b++) { // Go through each time slot
            let currentTimeSlot = Object.keys(fullSchedule[room][scheduleDay][b])[0];
            let currentTimeSlotContents = fullSchedule[room][scheduleDay][b][currentTimeSlot];

            if (currentTimeSlotContents == "") { // Found empty time, check if all other rooms offering at same time
                let conflictOffering: boolean = false;
                for (var e = 0; e < rooms.length; e++){ // Go through every room
                    let otherRoom = rooms[e]["rooms_shortname"] + " " + rooms[e]["rooms_number"];
                    let otherTimeSlot = Object.keys(fullSchedule[otherRoom][scheduleDay][b])[0];
                    let otherTimeSlotContents = fullSchedule[otherRoom][scheduleDay][b][otherTimeSlot];
                    if (otherTimeSlotContents.indexOf(courseShortName) != -1) {
                        conflictOffering = true;
                        break; // If we find a time that conflicts, we can stop searching through rooms
                    }
                }

                if (conflictOffering) {
                    continue; // This time has a conflict, let's check the next time slot
                } else {
                    foundATime = true;
                    fullSchedule[room][scheduleDay][b][currentTimeSlot] = courseName;
                    break; // We've found a valid time slot, we can stop searching time slots
                }
            }
        }
        return [foundATime, fullSchedule];
    }

    /**
     * Counts the number of time slots occupied by a course for a given room schedule interval
     * @param counter
     * @param roomSchedule
     * @param startTime
     * @param endTime
     * @returns {number}
     */
    private countCourses(counter: number, roomSchedule: any[], startTime: number, endTime: number): any {
        for (var v = startTime; v < endTime; v++){
            let timeSlot = Object.keys(roomSchedule[v])[0];
            let timeSlotContents = roomSchedule[v][timeSlot];
            if (timeSlotContents != "")
                counter++;
        }
        return counter;
    }

    /**
     * Calculates the quality for the schedule of each room and updates the full schedule with it
     * @param fullSchedule
     * @returns {any}
     */
    private scheduleQualityCheck(fullSchedule: any): any {
        var scheduleKeys = Object.keys(fullSchedule);
        for (var z = 0; z < scheduleKeys.length; z++) {
            let wholeDayCount = 0; // Count of all courses in schedule
            let outsideDayCount = 0; // Count of all courses outside regular hours in schedule

            let roomScheduleMWF = fullSchedule[scheduleKeys[z]]["mwf"];
            let roomScheduleTT = fullSchedule[scheduleKeys[z]]["tt"];

            // Get the courses count for whole day
            wholeDayCount = this.countCourses(wholeDayCount, roomScheduleMWF, 0, roomScheduleMWF.length);
            wholeDayCount = this.countCourses(wholeDayCount, roomScheduleTT, 0, roomScheduleTT.length);

            // Get the courses for early day
            outsideDayCount = this.countCourses(outsideDayCount, roomScheduleMWF, 0, 8);
            outsideDayCount = this.countCourses(outsideDayCount, roomScheduleTT, 0, 5);

            // Get the courses for late day
            outsideDayCount = this.countCourses(outsideDayCount, roomScheduleMWF, 17, 24);
            outsideDayCount = this.countCourses(outsideDayCount, roomScheduleTT, 11, 16);

            // Account for if good courses = 0
            fullSchedule[scheduleKeys[z]]["quality"] -= (outsideDayCount / wholeDayCount) * 100;

        }
        return fullSchedule;
    }

    /**
     * Takes a list of courses and a list of rooms to create a schedule
     * Returns courses that could not be scheduled and the schedule itself
     * @param courses
     * @param rooms
     * @returns {any[]}
     */
    public scheduleRooms(courses: any[], rooms: any[]): any {
        // Sort courses from largest section to smallest
        var sortedCourses: any = courses.sort(
            function(a: any, b: any): any {
                if ((a["courses_pass"] + a["courses_fail"]) < (b["courses_pass"] + b["courses_fail"])) return 1;
                if ((a["courses_pass"] + a["courses_fail"]) > (b["courses_pass"] + b["courses_fail"])) return -1;
                return 0;
            }
        );

        // Sort rooms from largest seats to smallest
        var sortedRooms: any = rooms.sort(
            function(a: any, b: any): any {
                if (a["rooms_seats"] < b["rooms_seats"]) return 1;
                if (a["rooms_seats"] > b["rooms_seats"]) return -1;
                return 0;
            }
        );


        var invalidScheduling: any = []; // List of courses that cannot fit into any rooms
        var fullSchedule: any = this.buildRoomSchedule(sortedRooms); // Full schedule containing every room

        invalidScheduling = this.findInvalidCourses(sortedCourses, sortedRooms, invalidScheduling);
        sortedCourses = this.removeInvalidCoures(sortedCourses, invalidScheduling);

        // Go through each course and try to find a room for each
        for (var j = 0; j < sortedCourses.length; j++) { // Go through courses
            var currentCourse = sortedCourses[j];
            var currentSectionSize: number = currentCourse["courses_pass"] + currentCourse["courses_fail"];
            var currentCourseShortName = currentCourse["courses_dept"].toUpperCase() + " " + currentCourse["courses_id"];
            var currentCourseName = currentCourse["courses_dept"].toUpperCase() + " " + currentCourse["courses_id"] +
                " " + currentCourse["courses_uuid"] + " - " + "Section Size: " + currentSectionSize.toString() ;

            // Sort diffs from smallest to largest
            var currentSizeDiffs = this.getRoomSizeDiffs(currentCourse, rooms); // Find size diffs for this course
            var sortedCurrentSizeDiffs =  currentSizeDiffs.sort(
                function(a: any, b: any) {
                    return a[1] - b[1];
                }
            );
            var foundATime: boolean = false; // Lets us know if we've found a valid time slot

            for (var a = 0; a < sortedCurrentSizeDiffs.length; a++) { // Go through valid rooms, check for good times
                let currentRoom: string = sortedCurrentSizeDiffs[a][0];

                // Try and find a time in MWF
                var goodDayMWF:any = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'mwf', 8, 17, foundATime);
                foundATime = goodDayMWF[0];
                fullSchedule = goodDayMWF[1];

                // If no time in MWF, check TT
                if (!foundATime) {
                    var goodDayTT:any = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'tt', 5, 11, foundATime);
                    foundATime = goodDayTT[0];
                    fullSchedule = goodDayTT[1];
                }

                if (foundATime)
                    break; // If we have found a good time, we can stop looking through rooms
            }

            // If we can't find a best time (8-17), find a early time (0-7)
            if (!foundATime) {
                for (var l = 0; l < sortedCurrentSizeDiffs.length; l++) {
                    let currentRoom: string = sortedCurrentSizeDiffs[l][0];

                    // Go through MWF
                    var earlyDayMWF = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'mwf', 0, 8, foundATime);
                    foundATime = earlyDayMWF[0];
                    fullSchedule = earlyDayMWF[1];

                    // If no time in MWF, check TT
                    if (!foundATime) {
                        var earlyDayTT = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'tt', 0, 5, foundATime);
                        foundATime = earlyDayTT[0];
                        fullSchedule = earlyDayTT[1];
                    }

                    if (foundATime)
                        break; // If we have found a early time, we can stop looking through rooms
                }
            }

            // If we can't find a early time (0-7), find a late time (17-23)
            if (!foundATime) {
                for (var q = 0; q < sortedCurrentSizeDiffs.length; q++) { // Going through each room
                    let currentRoom: string = sortedCurrentSizeDiffs[q][0];

                    // Go through MWF
                    var lateDayMWF = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'mwf', 17, 24, foundATime);
                    foundATime = lateDayMWF[0];
                    fullSchedule = lateDayMWF[1];

                    // If no time in MWF, check TT
                    if (!foundATime) {
                        var lateDayTT = this.findATime(currentCourseShortName, currentCourseName, sortedRooms, currentRoom, fullSchedule, 'tt', 11, 16, foundATime);
                        foundATime = lateDayTT[0];
                        fullSchedule = lateDayTT[1];
                    }

                    if (foundATime)
                        break; // If we have found a early time, we can stop looking through rooms
                }
            }

            // If we still haven't found a time, add stuff to invalid list
            if (!foundATime)
                invalidScheduling.push(sortedCourses[j]);
        }

        // Update fullSchedule with quality for each room schedule
        fullSchedule = this.scheduleQualityCheck(fullSchedule);


        return [invalidScheduling, fullSchedule];
    }

}