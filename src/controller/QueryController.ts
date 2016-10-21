/**
 * Created by rtholmes on 2016-06-19.
 */

import {Datasets} from "./DatasetController";
import Log from "../Util";
import DatasetController from '../controller/DatasetController';

export interface QueryRequest {
    GET: string|string[];
    WHERE: {};
    ORDER: any;
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
        if (query.GET == undefined || query.GET == null || query.AS == undefined || query.AS == null)
            return false;
        if (typeof query !== 'undefined' && query !== null && Object.keys(query).length > 0) {
            console.log('QueryController.isValid: Object.keys(query) is ' + Object.keys(query));
            return true;
        }
        return false;
    }

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
                        if (z == 0){
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
                    break;
                }
            }
        }
        finalArray = b1.concat(b2);
        return finalArray;
    }

    private processWhere(data: Array<any>, whereCondition:string, restriction:any, restrictionValue:any, notFlag : boolean = false, getRequests:any):any {
        let processedData: Array<any> = [];

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

    // private queryOrder(query: QueryRequest, unsortedData: Array<any>): any {
    //     var orderKey:any;
    //     if (query.ORDER == undefined)
    //         orderKey = "courses_dept";
    //     else {
    //         if (query.GET.indexOf(query.ORDER) >= 0)
    //             orderKey = query.ORDER;
    //         else
    //             return null;
    //     }
    //     var sortedData = unsortedData.sort(
    //         function(a,b): any {
    //             if (a[orderKey] < b[orderKey]) return -1;
    //             if (a[orderKey] > b[orderKey]) return 1;
    //             return 0;
    //         });
    //     // console.log('we are in queryOrder')
    //     // console.log(sortedData);
    //     return sortedData;
    // }


    // UP means lowest first
    // Down means highest first
    private queryOrder(query: QueryRequest, unsortedData: Array<any>): any {
        var orderKeys: any;
        var downDir: boolean = false;
        console.log("in query order")

        if (query.ORDER == undefined) {
            orderKeys = "courses_dept";
        }
        else if (typeof query.ORDER === 'string') {
            if (query.GET.indexOf(query.ORDER) >= 0)
                orderKeys = query.ORDER;
            else
                return null;
        }
        else if (typeof query.ORDER === 'object') {
            console.log(query.ORDER['dir']);
            if (query.ORDER['dir'] == 'DOWN')
                downDir = true;
            orderKeys = query.ORDER.keys;
            console.log(orderKeys);
            for (var key in orderKeys) {
                if (query.GET.indexOf(key))
                    continue;
                else
                    return null;
            }
        }
        var sortedData = unsortedData.sort(
            function comparator(a,b): any {
                var property: any;
                if (typeof orderKeys === 'string')
                    property = orderKeys;
                else
                    property = orderKeys[0];
                if (a[property] < b[property]) return -1;
                if (a[property] > b[property]) return 1;
                if (a[property] == b[property]){
                    for (var i = 1; i < orderKeys.length; i++) {
                        if (a[orderKeys[i]] < b[orderKeys[i]]) return -1;
                        if (a[orderKeys[i]] > b[orderKeys[i]]) return 1;
                        if (a[orderKeys[i]] == b[orderKeys[i]]) continue;
                    }
                    return 0;
                }
            });
        // console.log('We are in queryOrder')
        // console.log(sortedData);
        if (downDir)
            sortedData = sortedData.reverse();

        return sortedData;
    }

    private queryAs(query: QueryRequest, resultArray: Array<any>): any {
        if (resultArray == null)
            return null;
        if (query.AS === "TABLE") {
            var dataObject: any = {};
            dataObject['render'] = "TABLE";
            dataObject['result'] = resultArray;
        }
        return dataObject;
    }

    public query(query: QueryRequest): QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');

        // TODO: implement this (where we handle get, where, etc.)
        let queryResult:Array<any>;
        let completedWhereQuery:any;
        let completedOrderQuery:any;
        let dataset1:Array<any> = [];
        let dataset2:Array<any> = [];
        let controller = QueryController.datasetController;
        // For the get query
        if (query.GET){
            // console.log("inside : " + query.GET);
            queryResult = controller.queryDataset(query.GET);
            if (query.WHERE) {
                completedWhereQuery = this.queryWhere(query.WHERE, query.GET, queryResult, false, dataset1, dataset2);
                completedOrderQuery = this.queryOrder(query, completedWhereQuery);
            }
        }
        var resultToBeRendered = this.queryAs(query,completedOrderQuery);
        return resultToBeRendered;
    }
}
