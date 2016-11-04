/**
 * Created by rtholmes on 2016-09-03.
 */

import Log from "../Util";
import JSZip = require('jszip');
import {relative} from "path";
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
            if (fs.existsSync('data')) {
                let fileArray = fs.readdirSync('data');
                for (var i = 0; i < fileArray.length; i++) {
                    if (fileArray[i].includes("json")) {
                        if (fileArray[i] == "courses.json") {
                            console.log("Loading time");
                            this.datasets["courses"] = fs.readFileSync('data/courses.json', 'utf8');
                        }
                        console.log("Found one!" + this.datasets);
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
                    let tempDataset = {
                        courses: <Array<any>> [],
                    };

                    //  processedDataset['courses'] = [];
                    // for future reference if (id == "courses") then do this for loop
                    var i = 0;
                    var fileCount:number = 0;
                    zip.folder(id).forEach(function (relativePath, file) {
                        // check for dir
                        if (!file.dir) {
                            if (id == 'courses') {
                                fileCount++;
                            }
                            else {
                                fulfill(false);
                            }
                            file.async("string").then(function success(contents) {
                                if (id == 'courses') {
                                    var parsedData = JSON.parse(contents);

                                    if (parsedData.result.length != 0) {
                                        processedDataset.courses.push(parsedData.result);
                                    }
                                    if (i == (fileCount - 1)) {
                                        fulfill(true);
                                        that.save(id, processedDataset);
                                    }
                                    i++;
                                }

                            });
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
    //TODO: Update for D3 dataset
    private save(id:string, processedDataset:any) {

        if (!fs.existsSync('data')) {
            fs.mkdirSync('data');
        }
        if (id == "courses") {
            fs.writeFileSync('data/' + id + '.json', JSON.stringify(processedDataset.courses));
        }

        // if (id == "rooms") {
        //     fs.writeFileSync('data/' + id + '.json', JSON.stringify(processedDataset.rooms));
        // }

        this.datasets[id] = processedDataset;
    }

    public queryDataset(queryIDs:any) : Array<any> {
        this.getDatasets();
        let mainID:any;
        let coursesDataset:any;
        let parsedCDB:any;
        for (var i = 0; i < queryIDs.length; i++) {
            if (queryIDs[i].indexOf("_") >= 0){
                mainID = queryIDs[i     ].split("_")[0];
                coursesDataset = this.getDataset(mainID);
                parsedCDB = JSON.parse(coursesDataset);
            }
        }
        let currentSearchArray:Array<any> = [];
        for (var x = 0; x < parsedCDB.length; x++ ) {
            if (parsedCDB[x] != null) {
                switch (mainID) {
                    case 'courses':
                        for (var z = 0; z < parsedCDB[x].length; z++) {
                            let currentResult:any = {};
                            currentResult["courses_dept"] = parsedCDB[x][z].Subject;
                            currentResult["courses_id"] = parsedCDB[x][z].Course;
                            currentResult["courses_avg"] = parsedCDB[x][z].Avg;
                            currentResult["courses_instructor"] = parsedCDB[x][z].Professor;
                            currentResult["courses_title"] = parsedCDB[x][z].Title;
                            currentResult["courses_pass"] = parsedCDB[x][z].Pass;
                            currentResult["courses_fail"] = parsedCDB[x][z].Fail;
                            currentResult["courses_audit"] = parsedCDB[x][z].Audit;
                            currentResult["courses_uuid"] = parsedCDB[x][z].id;
                            // currentResult["courses_year"] = parsedCDB[x][z].Year unless section = overall
                            currentSearchArray.push(currentResult);
                        }
                        break;
                    case 'rooms':
                        for (var z = 0; z < parsedCDB[x].length; z++) {
                            let currentResult:any = {};
                            currentResult["rooms_fullname"] = parsedCDB[x][z].Subject;
                            currentResult["rooms_shortname"] = parsedCDB[x][z].Course;
                            currentResult["rooms_number"] = parsedCDB[x][z].Avg;
                            currentResult["rooms_name"] = parsedCDB[x][z].Professor;
                            currentResult["rooms_address"] = parsedCDB[x][z].Title;
                            currentResult["rooms_lat"] = parsedCDB[x][z].Pass;
                            currentResult["rooms_lon"] = parsedCDB[x][z].Fail;
                            currentResult["rooms_seats"] = parsedCDB[x][z].Audit;
                            currentResult["rooms_type"] = parsedCDB[x][z].id;
                            currentResult["rooms_furniture"] = parsedCDB[x][z].id;
                            currentResult["rooms_href"] = parsedCDB[x][z].id;
                            currentSearchArray.push(currentResult);
                        }

                        break;
                    default:
                        break;
                }
            }
        }
        return currentSearchArray;
    }

    public deleteDataset(id:string) {
        if (fs.existsSync("data/" + id + '.json')) {
            fs.unlinkSync("data/" + id + '.json');
            console.log('Done unlinkSync in DatasetController.ts');
        }
        if (this.datasets[id] !== undefined) {
            this.datasets[id] = undefined;
            console.log('Done setting cache (this.datasets) to undefined');
            console.log(this.datasets[id]);
        }
    }
}
