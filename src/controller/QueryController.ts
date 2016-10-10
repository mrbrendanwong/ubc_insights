/**
 * Created by rtholmes on 2016-06-19.
 */

import {Datasets} from "./DatasetController";
import Log from "../Util";
import DatasetController from '../controller/DatasetController';

export interface QueryRequest {
    GET: string|string[];
    WHERE: {};
    ORDER: string;
    AS: string;
    add?: number[];
    multiply?: number[];
}

export interface QueryResponse {
}

export default class QueryController {
    private static datasetController = new DatasetController();
    private datasets: Datasets = null;

    constructor(datasets: Datasets) {
        this.datasets = datasets;
    }

    public isValid(query: QueryRequest): boolean {
        if (typeof query !== 'undefined' && query !== null && Object.keys(query).length > 0) {
            console.log('QueryController.isValid: Object.keys(query) is ' + Object.keys(query));
            return true;
        }
        return false;
    }

    // TODO: Basic implementation of comparators. Phase 1: LT GT EQ
    // BY BRENDON
    // Handles key conditions of LT, GT, EQ comparators; OR, AND logic; IS, NOT
    // Take elements from result array. If meets filter requirements, add to a new array
    // If WHERE does not contain a filter, just skip and return original array
    // Cases for GT, LT, EQ, which involves {key : number}, refer to Deliverable1.md
    // In the case of GT, for each element in array, check key specified by comparator
    // Then check number. Assign to a variable
    // Do a for loop for all elements in the array, only adding elements to a new array if they meet conditions
    // In case of MCOMPARATORs, we should handle case where data is not numbers by just ignoring the filter (tentative)
    // Return new array
    private queryWhere(whereRequests:any, getRequests:any, rawData : Array<any>, notFlag:boolean,  dataset1 : Array<any> = [], dataset2: Array<any> = []): any {
        let whereID: Array<string>;
        let restriction: Array<string>;
        if (whereRequests.length == 0)
            return rawData;
        else {
            switch(Object.keys(whereRequests)[0]) {
                case 'GT':
                    restriction = Object.keys(whereRequests.GT);
                    whereID = whereRequests.GT[restriction.toString()];
                    return this.processWhere(rawData, 'GT', restriction, whereID, notFlag, getRequests);
                case 'LT':
                    restriction = Object.keys(whereRequests.LT);
                    whereID = whereRequests.LT[restriction.toString()];
                    return this.processWhere(rawData, 'LT', restriction, whereID, notFlag, getRequests);
                case 'EQ':
                    restriction = Object.keys(whereRequests.EQ);
                    whereID = whereRequests.EQ[restriction.toString()];
                    return this.processWhere(rawData, 'EQ', restriction, whereID, notFlag, getRequests);
                case 'IS':
                    restriction = Object.keys(whereRequests['IS']);
                    whereID = whereRequests['IS'][restriction.toString()];
                    return this.processWhere(rawData, 'IS', restriction, whereID, notFlag, getRequests);
                case 'AND':
                    // no not flag for AND or OR
                    dataset1 = this.queryWhere(whereRequests.AND[0], getRequests, rawData, notFlag);
                    dataset2 = this.queryWhere(whereRequests.AND[1], getRequests, dataset1, notFlag);
                    if (whereRequests.AND.length == 3) {
                        dataset1 = this.queryWhere(whereRequests.AND[2], getRequests, dataset2, notFlag);
                        return dataset1;
                    }
                    else
                        return dataset2;
                case 'OR':
                    dataset1 = this.queryWhere(whereRequests.OR[0], getRequests, rawData, notFlag);
                    dataset2 = this.queryWhere(whereRequests.OR[1], getRequests, rawData, notFlag);
                    //let combinedDataset:Array<any> = dataset1.concat(dataset2);
                    return this.unionArrays(dataset1,dataset2, getRequests);
                case 'NOT':
                    notFlag = true;
                    return this.queryWhere(whereRequests.NOT, getRequests, rawData, notFlag);
                default:
                    console.log("Unsupported WHERE request");
                    break;
            }
        }

    }

    private unionArrays(a1:any, a2:any, getRequests:any) :any {
        var finalArray:any;
        var b1:any = a1;
        var b2:any = a2;
        var c1:any;
        var allIdentical = true;
        for (var i = 0; i < b1.length; i++) {
            for (var x = 0; x < b2.length; x++) {
                allIdentical = true;
                for (var y = 0; y < getRequests.length; y++){
                    if (b1[i][getRequests[y]] != b2[x][getRequests[y]])
                        allIdentical = false;
                }
                if (allIdentical) {
                    c1 = b2.splice(x, 1);
                }
            }
        }
        finalArray = b1.concat(b2);
        return finalArray;
    }

    private processWhere(data: Array<any>, whereCondition:string, restriction:any, restrictionValue:any, notFlag : boolean = false, getRequests:any):any {
        let processedData: Array<any> = [];

      //  console.log("current process " + (notFlag[0] == ""));
        switch(whereCondition) {
            case 'GT':
                if (notFlag){
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] <= restrictionValue)
                            processedData.push(data[i]);
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] > restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                }
                break;
            case 'LT':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] >= restrictionValue)
                            processedData.push(data[i]);
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] < restrictionValue)
                            processedData.push(data[i]);
                    }
                }
                break;
            case 'EQ':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] !== restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                } else {
                    for (var i = 0; i < data.length; i++) {
                        if (data[i][restriction] === restrictionValue) {
                            processedData.push(data[i]);
                        }
                    }
                }
                break;
            case 'IS':
                if (notFlag) {
                    for (var i = 0; i < data.length; i++) {
                        if ((restrictionValue.localeCompare(data[i][restriction].toLowerCase())) != 0)
                            processedData.push(data[i]);
                    }
                } else {
                    if (restrictionValue[restrictionValue.length - 1] == "*" && restrictionValue[0] != "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction].toLowerCase()).startsWith(restrictionValue)) {
                                processedData.push(data[i]);
                            }
                        }
                    } else if (restrictionValue[0] == "*" && restrictionValue[restrictionValue.length - 1] != "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction].toLowerCase()).endsWith(restrictionValue)) {
                                processedData.push(data[i]);
                            }
                        }
                    } else if (restrictionValue[0] == "*" && restrictionValue[restrictionValue.length - 1] == "*" ) {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction].toLowerCase()).indexOf(restrictionValue) >= 0) {
                                processedData.push(data[i]);
                            }
                        }
                    }else {
                        for (var i = 0; i < data.length; i++) {
                            if (restrictionValue == (data[i][restriction].toLowerCase()))
                                processedData.push(data[i]);
                        }
                    }
                }
                break;
            default:
                console.log("Attempting to process unsupported WHERE query");
                break;
        }
        var filteredData = this.filterByGET(processedData, getRequests);
        return filteredData;
    }

    private filterByGET(unfinishedDataset: any, getRequests:any ) : any {
        var finalizedArray:any = [];
        for (var x = 0; x < unfinishedDataset.length; x++) {
            var currentResult: any = {};
            for (var z = 0; z < getRequests.length; z++) {
                let datasetID = getRequests[z].split("_")[0];
                let dataID = getRequests[z].split("_")[1];
                switch (datasetID) {
                    case 'courses':
                        switch (dataID) {
                            case 'dept':
                                currentResult["courses_dept"] = unfinishedDataset[x].courses_dept;
                                break;
                            case 'id':
                                currentResult["courses_id"] = unfinishedDataset[x].courses_id;
                                break;
                            case 'avg':
                                currentResult["courses_avg"] = unfinishedDataset[x].courses_avg;
                                break;
                            case 'instructor':
                                currentResult["courses_instructor"] = unfinishedDataset[x].courses_instructor;
                                break;
                            case 'title':
                                currentResult["courses_title"] = unfinishedDataset[x].courses_title;
                                break;
                            case 'pass':
                                currentResult["courses_pass"] = unfinishedDataset[x].courses_pass;
                                break;
                            case 'fail':
                                currentResult["courses_fail"] = unfinishedDataset[x].courses_fail;
                                break;
                            case 'audit':
                                currentResult["courses_audit"] = unfinishedDataset[x].courses_audit;
                                break;
                            default:
                                console.log("Uh oh, you sent an invalid key");
                                break;
                        }
                        break;
                    default:
                        break;
                }
            }
            finalizedArray.push(currentResult);
        }
        return finalizedArray;
    }


// TODO: Recieves the key (eg. courses_avg) we order by and the result array from GET; sort if key is in array
// BY BRENDON
// checks if key we're ordering by has data as string or number matching regex
// string ::= [a-zA-Z0-9,_-]+
// number ::= [1-9]*[0-9]+ ('.' [0-9]+ )?
//
// If string, first, check if string is exact same; if so, move on
// Else convert to string array, check if first letters of two elements, switch
// If first letters are the same, move on to second letters and so on
// If number, just straight up compare them; order from least to greatest
    private queryOrder(query: QueryRequest, unsortedData: Array<any>): any {
        var orderKey:any;
        if (query.ORDER == undefined)
            orderKey = "courses_dept";
        else
            orderKey = query.ORDER;
        var sortedData = unsortedData.sort(
            function(a,b): any {
                if (a[orderKey] < b[orderKey]) return -1;
                if (a[orderKey] > b[orderKey]) return 1;
                return 0;
            });
       // console.log('we are in queryOrder')
        // console.log(sortedData);
        return sortedData;
    }

// TODO: Read AS from query, returns what we should set "render" in data obj to
// BY BRENDON
// Checks what view we want, if table, returns data in some table format
// set render as table element as table
    private queryAs(query: QueryRequest, resultArray: Array<any>): any {
        if (query.AS === "TABLE") {
            var dataObject: any = {};
            dataObject['render'] = "TABLE";
            dataObject['result'] = resultArray;
        }
        return dataObject;
    }

// TODO: Create basic template to return full data object
// BY BRENDON
// Will perform the GET, call other parts of query (WHERE, ORDER, AS)
// Create full data object consisting of render:string and result:array
// Do GET, get our array with data specified with GET keys
// Pass array to WHERE so we can get filtered array
// Pass array to ORDER so we can sort by a key (eg. sort by courses_avg)
// Pass QueryRequest to AS, so we can set "render" in full object to "table" if matches query
    public query(query: QueryRequest): QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');

        // TODO: implement this (where we handle get, where, etc.)
        let testObject = {};
        let queryResult:Array<any>;
        let asdf:any;
        let fdsa:any;
        let dataset1:Array<any> = [];
        let dataset2:Array<any> = [];
        let notFlag: boolean;
        let controller = QueryController.datasetController;
        // For the get query
        if (query.GET){
           // console.log("inside : " + query.GET);
            queryResult = controller.queryDataset(query.GET);
            if (query.WHERE) {
                fdsa = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);

                asdf = this.queryOrder(query, fdsa);
            }
        }
        var qqqq = this.queryAs(query,asdf);
        return qqqq;
    }
}
