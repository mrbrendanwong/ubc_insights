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
    GROUP: string[];
    APPLY: any[];
    add?: number[];
    multiply?: number[];
}

export interface QueryResponse {
}

export default class QueryController {
    private static datasetController = new DatasetController();
    private datasets:Datasets = null;

    constructor(datasets:Datasets) {
        this.datasets = datasets;
    }

    public isValid(query:QueryRequest):boolean {
        if (query.GET == undefined || query.GET == null || query.AS == undefined || query.AS == null)
            return false;
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
    private queryWhere(whereRequests:any, getRequests:any, rawData:Array<any>, notFlag:boolean, dataset1:Array<any> = [], dataset2:Array<any> = []):any {
        let whereID:Array<string>;
        let restriction:Array<string>;
        if (whereRequests.length == 0)
            return rawData;
        else {
            switch (Object.keys(whereRequests)[0]) {
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
                    var previousData = rawData;
                    var currentData:any;
                    for (var i = 0; i < whereRequests.AND.length; i++) {
                        currentData = this.queryWhere(whereRequests.AND[i], getRequests, previousData, notFlag);
                        previousData = currentData;
                    }
                    return currentData;
                // dataset1 = this.queryWhere(whereRequests.AND[0], getRequests, rawData, notFlag);
                // dataset2 = this.queryWhere(whereRequests.AND[1], getRequests, dataset1, notFlag);
                //  if (whereRequests.AND.length == 3) {
                //      dataset1 = this.queryWhere(whereRequests.AND[2], getRequests, dataset2, notFlag);
                //     return dataset1;
                //    else
                //       return dataset2;
                case 'OR':
                    //dataset1 = this.queryWhere(whereRequests.OR[0], getRequests, rawData, notFlag);
                    //dataset2 = this.queryWhere(whereRequests.OR[1], getRequests, rawData, notFlag);
                    var datasetArray:any = [];
                    for (var i = 0; i < whereRequests.OR.length; i++) {
                        dataset1 = this.queryWhere(whereRequests.OR[i], getRequests, rawData, notFlag);
                        datasetArray.push(dataset1);
                    }
                    var oldDataset = {};
                    for (var z = 0; z < datasetArray.length; z++) {
                        if (z == 0) {
                            oldDataset = datasetArray[z];
                        } else {
                            oldDataset = this.unionArrays(oldDataset, datasetArray[z], getRequests);
                        }
                    }
                    return oldDataset;
                case 'NOT':
                    return this.queryWhere(whereRequests.NOT, getRequests, rawData, !notFlag);
                default:
                    console.log("Unsupported WHERE request");
                    return null;
            }
        }

    }

    private unionArrays(a1:any, a2:any, getRequests:any):any {
        var finalArray:any;
        var b1:any = a1;
        var b2:any = a2;
        var c1:any;
        var allIdentical = true;
        for (var i = 0; i < b1.length; i++) {
            for (var x = 0; x < b2.length; x++) {
                allIdentical = true;
                for (var y = 0; y < getRequests.length; y++) {
                    if (b1[i][getRequests[y]] != b2[x][getRequests[y]])
                        allIdentical = false;
                }
                if (allIdentical) {
                    c1 = b2.splice(x, 1);
                    break;
                }
            }
        }
        finalArray = b1.concat(b2);
        return finalArray;
    }

    private processWhere(data:Array<any>, whereCondition:string, restriction:any, restrictionValue:any, notFlag:boolean = false, getRequests:any):any {
        let processedData:Array<any> = [];

        switch (whereCondition) {
            case 'GT':
                if (notFlag) {
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
                    } else if (restrictionValue[0] == "*" && restrictionValue[restrictionValue.length - 1] == "*") {
                        restrictionValue = restrictionValue.replace(/\*/g, '');
                        for (var i = 0; i < data.length; i++) {
                            if ((data[i][restriction].toLowerCase()).indexOf(restrictionValue) >= 0) {
                                processedData.push(data[i]);
                            }
                        }
                    } else {
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
        // call this later
        //  var filteredData = this.filterByGET(processedData, getRequests);
        return processedData;
    }

    private filterByGET(unfinishedDataset:any, getRequests:any, applyRequests:any = []):any {
        var finalizedArray:any = [];
        for (var x = 0; x < unfinishedDataset.length; x++) {
            var currentResult:any = {};
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
                            case 'uuid':
                                currentResult["courses_uuid"] = unfinishedDataset[x].courses_uuid;
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
    private queryOrder(query:QueryRequest, tobeSortedData:Array<any>):any {
        console.log(tobeSortedData);
        var orderKey:any;
        let unsortedData:any;
        if (query.ORDER == undefined)
            orderKey = "courses_dept";
        else {
            if (query.GET.indexOf(query.ORDER) >= 0)
                orderKey = query.ORDER;
            else
                return null;
        }
        if (query.GROUP == undefined)
            unsortedData = this.filterByGET(tobeSortedData, query.GET);
        else
            unsortedData = tobeSortedData;
        var sortedData = unsortedData.sort(
            function (a:any, b:any):any {
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
    private queryAs(query:QueryRequest, resultArray:Array<any>):any {
        if (resultArray == null)
            return null;
        if (query.AS === "TABLE") {
            var dataObject:any = {};
            dataObject['render'] = "TABLE";
            dataObject['result'] = resultArray;
        }
        return dataObject;
    }

// TODO: Finish GROUP
    private queryGroup(groupRequests:any, dataset:any, getRequests:any):any {

        let groupedDataset:any = [];
        let tempGroup:any = [];
        // For every offering
        for (var x = 0; x < dataset.length; x++) {
            if (x == 0)
                continue;
            if (this.shouldBeGrouped(dataset[x - 1], dataset[x], groupRequests))
                tempGroup.push(dataset[x - 1]);
            else {
                tempGroup.push(dataset[x - 1]);
                groupedDataset.push(tempGroup);
                tempGroup = [];
            }

        }
        return groupedDataset;

    }

    //private assembleOfferings(dataset:any):any {
    //    let combinedCourseArray:any = [];
    //    let tempArray:any = [];
    //    for (var i = 0; i < dataset.length; i++){
    //        if (i == 0)
    //            continue;
    //        if ((dataset[i].courses_dept == dataset[i-1].courses_dept) && (dataset[i].courses_id == dataset[i-1].courses_id))
    //            tempArray.push(dataset[i-1]);
    //        else {
    //            let tempObject:any = {};
    //            tempObject['courses_dept'] = tempArray[0].courses_dept;
    //            tempObject['courses_id'] =  tempArray[0].courses_id;
    //
    //            for (var x = 0; x < tempArray.length; x++) {
    //
    //            }
    //        }
    //    }
    //}
    // Verifies if two course offerings should be grouped together
    private shouldBeGrouped(offering1:any, offering2:any, groupRequests:any):boolean {
        let groupWorthy:boolean = true;
        for (var x = 0; x < groupRequests.length; x++) {
            if (offering1[groupRequests[x]] != offering2[groupRequests[x]])
                groupWorthy = false;
        }
        return groupWorthy;
    }

// TODO: Handle apply calls
    private queryApply(applyRequests:any, groupRequests:any, groupedDataset:any):any {
        // Go through each set of applications
        let appliedDataset:any = [];
        for (var i = 0; i < applyRequests.length; i++) {
            for (var x = 0; x < groupedDataset.length; x++)
                appliedDataset.push(this.applyComputations(applyRequests[i], groupRequests, groupedDataset[x]));
            // Send each group to the computation helper
        }
        //   console.log(appliedDataset);

        return appliedDataset;

    }

    private applyComputations(applyKey:any, groupRequests:any, dataInstance:any):any {
        // datainstance is an array of offerings (corresponding to a group)
        var trueApplyKey = applyKey[Object.keys(applyKey)[0]];
        //  console.log(JSON.stringify(Object.keys(applyKey)[0]));
        let computatedObject:any = {};
        let desiredID:any = "";
        for (var i = 0; i < groupRequests.length; i++) {
            computatedObject[groupRequests[i]] = dataInstance[0][groupRequests[i]];
        }
        //  console.log("HELLO " + JSON.stringify(applyKey[Object.keys(applyKey)[0]]));
        if (applyKey.length == 0)
            return dataInstance;
        switch (Object.keys(trueApplyKey)[0]) {
            case 'AVG':
                let sum = 0;
                let count = dataInstance.length - 1;
                for (var x = 0; x < dataInstance.length; x++) {
                    sum += dataInstance[x].courses_avg;
                }
                let result = sum / count;
                //  console.log("AVERAGE IS " + result.toFixed(2));
                computatedObject[Object.keys(applyKey)[0]] = result.toFixed(2);
                break;
            case 'COUNT':
                // figure something out
                computatedObject[Object.keys(applyKey)[0]] = dataInstance.length - 1;
                break;
            case 'MAX':
                let maxValue = 0;
                desiredID = applyKey['MAX'];
                for (var x = 0; x < dataInstance.length; x++) {
                    if (dataInstance[x][desiredID] > maxValue)
                        maxValue = dataInstance[x][desiredID];
                }
                computatedObject[Object.keys(applyKey)[0]] = maxValue;
                break;
            case 'MIN':
                let minValue = 1000;
                desiredID = applyKey['MIN'];
                for (var x = 0; x < dataInstance.length; x++) {
                    if (dataInstance[x][desiredID] < minValue)
                        minValue = dataInstance[x][desiredID];
                }
                computatedObject[Object.keys(applyKey)[0]] = minValue;
                break;
            default:
                console.log("Uh oh, this method received an unsupported APPLY key");
                break;
        }
        return computatedObject;
    }

// TODO: Create basic template to return full data object
// BY BRENDON
// Will perform the GET, call other parts of query (WHERE, ORDER, AS)
// Create full data object consisting of render:string and result:array
// Do GET, get our array with data specified with GET keys
// Pass array to WHERE so we can get filtered array
// Pass array to ORDER so we can sort by a key (eg. sort by courses_avg)
// Pass QueryRequest to AS, so we can set "render" in full object to "table" if matches query
    public query(query:QueryRequest):QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');

        // TODO: implement this (where we handle get, where, etc.)
        let queryResult:Array<any>;
        let completedWhereQuery:any;
        let completedOrderQuery:any;
        let completedGroupQuery:any;
        let completedApplyQuery:any;
        let dataset1:Array<any> = [];
        let dataset2:Array<any> = [];
        let controller = QueryController.datasetController;
        var resultToBeRendered:any;
        //if (query.GROUP.length == 0)
        // return something bad
        // else
        // controll.queryGroup();

        // For the get query
        if (query.GET) {
            // #D1 support
            queryResult = controller.queryDataset(query.GET);
            if (query.GROUP == undefined || query.GROUP.length == 0) {
                if (query.WHERE) {
                    completedWhereQuery = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);
                    completedOrderQuery = this.queryOrder(query, completedWhereQuery);
                }
                resultToBeRendered = this.queryAs(query, completedOrderQuery);
                return resultToBeRendered;
            } else {
                if (query.WHERE) {
                    completedWhereQuery = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);
                    completedGroupQuery = this.queryGroup(query.GROUP, completedWhereQuery, query.GET);
                    completedApplyQuery = this.queryApply(query.APPLY, query.GROUP, completedGroupQuery);
                    completedOrderQuery = this.queryOrder(query, completedApplyQuery);
                }
                resultToBeRendered = this.queryAs(query, completedOrderQuery);
                return resultToBeRendered;
            }
        }
    }
}
// TODO: Fix for D1 and talk about D2