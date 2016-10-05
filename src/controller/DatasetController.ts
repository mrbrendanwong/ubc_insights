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
                        return true;
                    }
                    break;
                default:
                    break;
            }
        }
        return null;
    }

    public getDatasets():Datasets {
        // TODO: if datasets is empty, load all dataset files in ./data from disk
        if (Object.keys(this.datasets).length == 0 || this.datasets == null || this.datasets == undefined) {
            //  //console.log(fs.existsSync('data'));
            if (fs.existsSync('data')) {
                fs.readdir('data', function (err:any, files:any):any {
                    if (err) {
                        console.error("Error");
                        return null;
                    }
                })
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
                   // reject("Hello mom");


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
}