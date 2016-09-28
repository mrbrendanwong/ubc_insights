/**
 * Created by rtholmes on 2016-09-03.
 */

import Log from "../Util";
import JSZip = require('jszip');
import resolve = require("resolve");
var fs = require("fs");
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
        // TODO: this should check if the dataset is on disk in ./data if it is not already in memory.
        return this.datasets[id];
    }

    public getDatasets():Datasets {
        // TODO: if datasets is empty, load all dataset files in ./data from disk

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
                    let processedDataset = {};
                    // TODO: iterate through files in zip (zip.files)
                    // The contents of the file will depend on the id provided. e.g.,
                    // some zips will contain .html files, some will contain .json files.
                    // You can depend on 'id' to differentiate how the zip should be handled,
                    // although you should still be tolerant to errors.

                    if (id == 'courses') {

                        processedDataset['courses'] = [];
                        // for future reference if (id == "courses") then do this for loop
                        var i = 0;
                        zip.folder("courses").forEach(function (relativePath, file) {
                            // check for dir
                            if (!file.dir) {

                                var currentCourse = {};
                                currentCourse['dept'] = relativePath.substring(0, 4);
                                currentCourse['id'] = relativePath.substring(4, relativePath.length);
                                currentCourse['info'] = "";
                                //console.log(JSON.stringify(file));
                                file.async("string").then(function success(contents) {
                                    var test = JSON.stringify(contents);
                                    var test2 = JSON.parse(contents);
                                    processedDataset.courses[i].info = test2.result;
                                    // console.log(JSON.stringify(processedDataset.courses[i].dept));
                                    // console.log(processedDataset.courses.length);
                                    if (i == (processedDataset.courses.length - 1)) {
                                        console.log(JSON.stringify(processedDataset.courses[i]));
                                        that.save(id, processedDataset);
                                        fulfill(true);
                                    }
                                    i++;
                                });

                                processedDataset.courses.push(currentCourse);
                            }
                        });
                    }

                    //Promise.all().then{
                    //
                    //}
                    //setTimeout(function() {
                    //    console.log("DONE");
                    //    that.save(id, processedDataset);
                    //}, 20000);




                    //   console.log(JSON.stringify(processedDataset));
                    //var allCourseArray = zip.folder("courses").file(/CPSC/);
                    //for (var i = 0; i < allCourseArray.length; i++ )
                    //{
                    //  //  console.log(allCourseArray[i].name); // Gets name in format "courses/courseName"
                    ////    console.log(JSON.stringify(allCourseArray[i]));
                    //    currentCourse.dept.push(allCourseArray[i].name.substring(allCourseArray[i].name.indexOf("/") + 1).substring(0,4));
                    //    currentCourse.id.push(allCourseArray[i].name.substring(allCourseArray[i].name.indexOf("/") + 1).substring(4,allCourseArray[i].name.length - 8));
                    //
                    //    allCourseArray[i].async("string")
                    //        .then(function success(content) {
                    //            //    console.log(content);
                    //          //  console.log(JSON.stringify(currentCourse));
                    //           // var test = JSON.stringify(content);
                    //            var test2 = JSON.parse(content);
                    //            currentCourse.info.push(test2.result);
                    //            console.log(JSON.stringify(test2));
                    //            //  console.log(JSON.stringify(currentCourse.info));
                    //            //console.log(JSON.stringify(currentCourse)
                    //        },      function error(e) {
                    //            console.log("hola");
                    //        });
                    //
                    //}
                    //  console.log(JSON.stringify(currentCourse));
                    // console.log("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")


                }).catch(function (err) {
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
        // add it to the memory m
        // fs unlinkodel
        fs.writeFileSync('courses.json', JSON.stringify(processedDataset.courses));
        console.log("THIS SHIT IS DONE");
        this.datasets[id] = processedDataset;
    }

}