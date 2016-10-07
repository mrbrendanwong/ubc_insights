/**
 * Created by rtholmes on 2016-09-03.
 */

import Log from "../Util";
import JSZip = require('jszip');
//import resolve = require("resolve");
var fs = require("fs");
var path = require("path");
/**
 * In memory representation of all datasets.
 */
export interface Datasets {
    [id: string]: {};
    //  result: Object; // change to object
}

export default class DatasetController {

    private datasets:Datasets = {};

    constructor() {
        Log.trace('DatasetController::init()');
    }

    /**
     * Returns the referenced dataset. If the dataset is not in memory, it should be
     * loaded from disk and put in memory. If it is not in disk, then it should return
     * null.
     *
     * @param id
     * @returns {{}}
     */
    public getDataset(id:string):any {
        this.getDatasets();
        // TODO: this should check if the dataset is on disk in ./data if it is not already in memory.
        if (this.datasets[id] == null || this.datasets[id] == undefined) {
            switch (id) {
                case 'courses':
                    if (fs.existsSync('data/' + id + ".json")) {
                        //this.datasets[id] = fs.readFileSync('data/' + id + '.json', 'utf8');
                        return this.datasets[id];
                    }
                    else
                        return null;
                default:
                    break;
            }
        } else {
            return this.datasets[id];
        }
        return null;
    }

    public getDatasets():Datasets {
        //TODO: if datasets is empty, load all dataset files in ./data from disk
        if (Object.keys(this.datasets).length == 0 || this.datasets == null || this.datasets == undefined) {
            //  //console.log(fs.existsSync('data'));
            if (fs.existsSync('data')) {
                let fileArray = fs.readdirSync('data');
                for (var i = 0; i < fileArray.length; i++) {
                    if (fileArray[i].includes("json")) {
                        if (fileArray[i] == "courses.json") {
                            console.log("Loading time");
                            this.datasets["courses"] = fs.readFileSync('data/courses.json', 'utf8');
                        }
                        console.log("Found one!");
                        return this.datasets;
                    }
                }
                return null;
            }
            else {
                return null;
            }
        }
        return this.datasets;
    }

    /**
     * Process the dataset; save it to disk when complete.
     *
     * @param id
     * @param data base64 representation of a zip file
     * @returns {Promise<boolean>} returns true if successful; false if the dataset was invalid (for whatever reason)
     */
    public process(id:string, data:any):Promise<boolean> {
        Log.trace('DatasetController::process( ' + id + '... )');

        let that = this;
        return new Promise(function (fulfill, reject) {
            try {
                let myZip = new JSZip();
                myZip.loadAsync(data, {base64: true}).then(function (zip:JSZip) {
                    Log.trace('DatasetController::process(..) - unzipped');

                    // switch to var
                    let processedDataset = {
                        courses: <Array<any>> [],
                    };
                    // TODO: iterate through files in zip (zip.files)
                    // The contents of the file will depend on the id provided. e.g.,
                    // some zips will contain .html files, some will contain .json files.
                    // You can depend on 'id' to differentiate how the zip should be handled,
                    // although you should still be tolerant to errors.


                    //  processedDataset['courses'] = [];
                    // for future reference if (id == "courses") then do this for loop
                    var i = 0;
                    zip.folder(id).forEach(function (relativePath, file) {
                        // check for dir
                        if (!file.dir) {
                            if (id == 'courses') {
                                var currentCourse = {
                                    dept: <string> null,
                                    id:<string> null,
                                    info:<string> null,
                                };
                                currentCourse.dept = relativePath.substring(0, 4);
                                currentCourse.id = relativePath.substring(4, relativePath.length);
                                currentCourse.info = "";
                            }
                            else {
                                fulfill(false);
                            }
                            file.async("string").then(function success(contents) {
                                if (id == 'courses') {
                                    var test = JSON.stringify(contents);
                                    var test2 = JSON.parse(contents);
                                    if (test2.result != "")
                                        if (i < 10)
                                            console.log("I is " + i);
                                    processedDataset.courses[i].info = test2.result;
                                    if (i == (processedDataset.courses.length - 1)) {
                                        fulfill(true);
                                        that.save(id, processedDataset);
                                    }
                                }
                                i++;
                            });
                            if (id == 'courses') {
                                processedDataset.courses.push(currentCourse);
                            }
                        }
                    });

                }).catch(function (err:any) {
                    Log.trace('DatasetController::process(..) - unzip ERROR: ' + err.message);
                    reject(err);
                });
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject(err);
            }
        });
    }

    /**
     * Writes the processed dataset to disk as 'id.json'. The function should overwrite
     * any existing dataset with the same name.
     *
     * @param id
     * @param processedDataset
     */
    private save(id:string, processedDataset:any) {

        if (!fs.existsSync('data')) {
            fs.mkdirSync('data');
        }
        if (id == "courses") {
            fs.writeFileSync('data/' + id + '.json', JSON.stringify(processedDataset.courses));
        }
        this.datasets[id] = processedDataset;
    }

    // FIRST DRAFT: SPENCER DID THIS

    public queryDataset(queryIDs:any) : Array<any> {
        this.getDatasets();
        let coursesDataset = this.getDataset(queryIDs[0].split("_")[0]);
        let parsedCDB = JSON.parse(coursesDataset);
        let currentSearchArray:Array<any> = [];
        for (var x = 0; x < parsedCDB.length; x++ ) {
            if (parsedCDB[x].info === undefined) {
                console.log("Skipped");
                continue;
            }
            for (var z = 0; z < parsedCDB[x].info.length; z++) {
                let currentResult = {};
                for (var i = 0; i < queryIDs.length; i++) {
                    let datasetID = queryIDs[i].split("_")[0];
                    let dataID = queryIDs[i].split("_")[1];
                    switch (datasetID) {
                        case 'courses':
                            switch (dataID) {
                                case 'dept':
                                    currentResult["courses_dept"] = parsedCDB[11].dept;
                                    break;
                                case 'id':
                                    currentResult["courses_id"] = parsedCDB[x].id;
                                    break;
                                case 'avg':
                                    currentResult["courses_avg"] = parsedCDB[x].info[z].Avg;
                                    break;
                                case 'instructor':
                                    currentResult["courses_instructor"] = parsedCDB[x].info[z].Professor;
                                    break;
                                case 'title':
                                    currentResult["courses_title"] = parsedCDB[x].info[z].Title;
                                    break;
                                case 'pass':
                                    currentResult["courses_pass"] = parsedCDB[x].info[z].Pass;
                                    break;
                                case 'fail':
                                    currentResult["courses_fail"] = parsedCDB[x].info[z].Fail;
                                    break;
                                case 'audit':
                                    currentResult["courses_audit"] = parsedCDB[x].info[z].Audit;
                                    break;
                                default:
                                    console.log("Uh oh, you sent an invalid key");
                                    break;
                            }
                            break;
                        default:
                            break;
                    }
                    currentSearchArray.push(currentResult);
                }
            }
        }
        return currentSearchArray;
    }

    // FIRST DRAFT: SPENCER DID THIS
    public searchForKey(key:string, parsedData:any) {
        switch (key) {
            case 'dept':
                console.log("Looking for dept");
                for (var i = 0; i < parsedData.length; i++) {
                    console.log(parsedData[i].dept);
                }
                break;
            case 'id':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].id);
                }
                break;
            case 'avg':
                let count = 0;
                for (var i = 0; i < parsedCDB.length; i++) {
                    // If this course has no offerings, skip it.
                    if (parsedCDB[i].info.length == 0)
                        continue;
                    let courseAvg = 0;
                    // handle undefined
                    //console.log(parsedCDB[i].info.length);
                    for (var x = 0; x < parsedCDB[i].info.length; x++) {
                        courseAvg = parsedCDB[i].info[x].Avg;
                        if (courseAvg > 90) {
                            count++;
                            //console.log("Average is " + parsedCDB[i].info[x].Avg + " for " + parsedCDB[i].dept);
                        }
                        //  console.log(x);
                    }
                    //   if ((x != 0) && ((avgSum/x) > 90 ))
                    //    console.log("Average is " + (avgSum/x) + " for " + parsedCDB[i].dept);
                }
                //   console.log("Count is " + count);
                break;
            case 'instructor':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].info[0].professor);
                }
                break;
            case 'title':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].info[0].title);
                }
                break;
            case 'pass':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].info[0].pass);
                }
                break;
            case 'fail':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].info[0].fail);
                }
                break;
            case 'audit':
                for (var i = 0; i < parsedCDB.length; i++) {
                    console.log(parsedCDB[i].info[0].audit);
                }
                break;
            default:
                console.log("Uh oh, you sent an invalid key");
                break;
        }
    }

    // FIRST DRAFT: BRENDON DID THIS
    public deleteDataset(id:string) {
        if (fs.existsSync("data/" + id + '.json')) {
            fs.unlinkSync("data/" + id + '.json');
            console.log('done unlinkSync in DatasetController.ts');
        }
        if (this.datasets[id] !== undefined) { // NEED TO UPDATE
            this.datasets[id] = undefined; // NEED TO UPDATE
            console.log('done setting cache (this.datasets) to undefined');
            console.log(this.datasets[id]);
        }
    }
}