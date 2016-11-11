/**
 * Created by rtholmes on 2016-09-03.
 */

import Log from "../Util";
import JSZip = require('jszip');
import {relative} from "path";
//import resolve = require("resolve");
var fs = require("fs");
var path = require("path");
var parse5 = require('parse5');
import { ASTNode as ASTNode } from "parse5";
import { ASTAttribute as ASTAttribute } from "parse5";
import http = require('http');
import {bodyParser} from "restify";

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
                case 'rooms':
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
                            console.log("Loading courses");
                            this.datasets["courses"] = fs.readFileSync('data/courses.json', 'utf8');
                        } else if (fileArray[i] == "rooms.json") {
                            console.log("Loading rooms");
                            this.datasets["rooms"] = fs.readFileSync('data/rooms.json', 'utf8');
                        }
                    }
                }
                console.log("Found one!" + this.datasets);
                return this.datasets;
                //  return null;
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
                        rooms: <Array<any>> []
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
                    switch(id){
                        case 'courses':
                            zip.folder(id).forEach(function (relativePath, file) {
                                // check for dir
                                if (!file.dir) {
                                    fileCount++;
                                    file.async("string").then(function success(contents) {
                                        var parsedData = JSON.parse(contents);

                                        if (parsedData.result.length != 0) {
                                            processedDataset.courses.push(parsedData.result);
                                        }
                                        if (i == (fileCount - 1)) {
                                            fulfill(true);
                                            that.save(id, processedDataset);
                                        }
                                        i++;
                                    });
                                }
                            });
                            break;
                        case 'rooms':
                            var validBuildings:any = [];
                            zip.file("index.htm").async("string").then(function success(contents) {
                                var document = parse5.parse(contents);
                                var previousValue:any;
                                function printNode(node: ASTNode) {
                                    if (node.attrs) {
                                        node.attrs.forEach(function (value: ASTAttribute) {
                                            previousValue = value.value;
                                        });
                                    }

                                    if (node.value && previousValue == 'views-field views-field-field-building-code') {
                                        if (node.value.trim() != "Code" && node.value.trim() != "")
                                            validBuildings.push(node.value.trim());
                                    }

                                    if (node.childNodes) {
                                        node.childNodes.forEach(printNode);
                                    }
                                }
                                printNode(document);
                                that.parseValidBuildings(zip, validBuildings).then(function(value) {
                                    processedDataset['rooms'] = value;
                                    fulfill(true);
                                    that.save(id, processedDataset);
                                });
                            });
                            break;
                        default:
                            fulfill(false);
                    }
                    // TODO: Have it read the index.htm file, then read only the html files that correspond to buildings in index.htm


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

    private parseValidBuildings(zip:JSZip, validBuildings:any):Promise<any> {
        let that = this;
        let outOfIdeasCounter:number = 0;
        let latLonCounter:number = 0;
        let asyncIterationCounter:number = 0;
        let buildingInfoCounter:number = 0;
        let roomInfoCounter:number = 0;
        let parsedArray:any = [];
        let buildingInfoCollectionFlag:boolean = false;
        let roomInfoCollectionFlag:boolean = false;
        let currentBuilding:any = "";
        let currentAddress:any = "";
        let currentLink:any = "";
        let currentCode:any = "";
        return new Promise(function(resolve) {
            for (var i = 0; i < validBuildings.length; i++) {
                let iterationObject:any = {};
                // parsedArray.push(iterationObject);
                //if (i == 0) {
                zip.file('campus/discover/buildings-and-classrooms/' + validBuildings[i]).async("string").then(function success(contents) {
                    var document = parse5.parse(contents);

                    function printNode(node:ASTNode) {

                        if (node.attrs) {
                            node.attrs.forEach(function (value:ASTAttribute) {
                                if (value.name == "href" && value.value.trim().length < 5)
                                    currentCode = value.value.trim();
                                if (roomInfoCollectionFlag && value.name == "href") {
                                    currentLink = value.value.trim();
                                }
                                if (value.value.trim() == "building-info")
                                    buildingInfoCollectionFlag = true;
                                else if (value.value.trim().indexOf("-room-") >= 0) {
                                    roomInfoCollectionFlag = true;
                                }
                            });
                        }

                        if (node.value) {
                            if (node.value.trim() != "Room" && node.value.trim() != "Capacity" && node.value.trim() != "Furniture type" && node.value.trim() != "Room type") {
                                if (buildingInfoCollectionFlag) {
                                    if (buildingInfoCounter < 2) {
                                        if (node.value.trim() != "") {
                                            buildingInfoCounter == 0 ? (currentBuilding = node.value.trim()) : (currentAddress = node.value.trim());
                                            buildingInfoCounter++;
                                        }
                                    } else {
                                        buildingInfoCollectionFlag = false;
                                        buildingInfoCounter = 0;
                                    }
                                } else if (roomInfoCollectionFlag) {
                                    if (roomInfoCounter < 4) {
                                        if (node.value.trim() != "") {
                                            if (roomInfoCounter == 0) {
                                                parsedArray.push({
                                                    rooms_fullname: currentBuilding,
                                                    rooms_shortname: currentCode,
                                                    rooms_address: currentAddress,
                                                    rooms_href: currentLink
                                                });
                                                that.getLatLon(currentAddress).then(function success(contents) {
                                                    parsedArray[latLonCounter]['rooms_lat'] = contents.lat;
                                                    parsedArray[latLonCounter]['rooms_lon'] = contents.lon;
                                                    if (latLonCounter == parsedArray.length -1)
                                                        resolve(parsedArray);
                                                    latLonCounter++;
                                                });
                                                parsedArray[asyncIterationCounter]['rooms_number'] = node.value.trim();
                                                parsedArray[asyncIterationCounter]['rooms_name'] = parsedArray[asyncIterationCounter]['rooms_shortname'] + " " + parsedArray[asyncIterationCounter]['rooms_number'];
                                            }
                                            else if (roomInfoCounter == 1)
                                                parsedArray[asyncIterationCounter]['rooms_seats'] = parseInt(node.value.trim());
                                            else if (roomInfoCounter == 2)
                                                parsedArray[asyncIterationCounter]['rooms_furniture'] = node.value.trim();
                                            else if (roomInfoCounter == 3)
                                                parsedArray[asyncIterationCounter]['rooms_type'] = node.value.trim();
                                            roomInfoCounter++;
                                        }
                                    } else {
                                        if (roomInfoCounter == 4)
                                            asyncIterationCounter++;
                                        roomInfoCounter = 0;
                                        roomInfoCollectionFlag = false;
                                    }
                                }
                            }
                        }

                        if (node.childNodes) {
                            node.childNodes.forEach(printNode);
                        }
                    }

                    printNode(document);
                    //if (outOfIdeasCounter == validBuildings.length - 1) {
                    //    console.log(parsedArray.length);
                    //    resolve(parsedArray);
                    //}
                    outOfIdeasCounter++;
                });
                //}
            }
        });
    }

// http://stackoverflow.com/questions/6968448/where-is-body-in-a-nodejs-http-get-response
    // https://nodejs.org/api/http.html
    // NOTE TO SPENCER: Dunno if I'm doing this right lol
    public getLatLon(buildingAddress: any): Promise<any> {
        let escapedAddress = encodeURIComponent(buildingAddress);
        let path = '/api/v1/team16/' +  escapedAddress;
        let options = {
            host: 'skaha.cs.ubc.ca',
            port: 8022,
            path: path,
        };

        return new Promise(function (resolve) {
            http.get(options, function(res) {
                var body = '';
                res.on('data', function(chunk: any) {
                    body += chunk;
                });
                res.on('end', function() {
                    resolve(JSON.parse(body));
                });
            }).on('error', function(e: any) {
                console.log("Got error: " + e.message);
            });
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
        } else if (id == "rooms") {
            fs.writeFileSync('data/' + id + '.json', JSON.stringify(processedDataset.rooms));
        }
        this.datasets[id] = processedDataset;
    }

    public queryDataset(queryIDs:any) : Array<any> {

        this.getDatasets();
        let mainID:any;
        let coursesDataset:any;
        let parsedCDB:any;
        for (var i = 0; i < queryIDs.length; i++) {
            if (queryIDs[i].indexOf("_") >= 0){
                mainID = queryIDs[i].split("_")[0];
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
                        return parsedCDB;
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
