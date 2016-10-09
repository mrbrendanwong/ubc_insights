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
    private queryWhere(whereRequests:any ,rawData : Array<any>, dataset1 : Array<any> = [], dataset2: Array<any> = []): any {
        let whereID: Array<string>;
        let restriction: Array<string>;
        if (whereRequests.length == 0)
            return rawData;
        else {
            switch(Object.keys(whereRequests)[0]) {
                case 'GT':
                    restriction = Object.keys(whereRequests.GT);
                    whereID = whereRequests.GT[restriction.toString()];
                    return this.processWhere(rawData, 'GT', restriction, whereID);
                case 'LT':
                    restriction = Object.keys(whereRequests.LT);
                    whereID = whereRequests.LT[restriction.toString()];
                    return this.processWhere(rawData, 'LT', restriction, whereID);
                case 'EQ':
                    restriction = Object.keys(whereRequests.EQ);
                    whereID = whereRequests.EQ[restriction.toString()];
                    return this.processWhere(rawData, 'EQ', restriction, whereID);
                case 'IS':
                    restriction = Object.keys(whereRequests['IS']);
                    whereID = whereRequests['IS'][restriction.toString()];
                    return this.processWhere(rawData, 'IS', restriction, whereID);
                case 'AND':
                    dataset1 = this.queryWhere(whereRequests.AND[0], rawData);
                    dataset2 = this.queryWhere(whereRequests.AND[1], dataset1);
                    return dataset2;
                case 'OR':
                    dataset1 = this.queryWhere(whereRequests.OR[0], rawData);
                    dataset2 = this.queryWhere(whereRequests.OR[1], rawData);
                    let combinedDataset:Array<any> = dataset1.concat(dataset2);
                    return combinedDataset;
                default:
                    console.log("Unsupported WHERE request");
                    break;
            }
        }

    }

    private processWhere(data: Array<any>, whereCondition:string, restriction:any, restrictionValue:any):any {
        let processedData: Array<any> = [];
        switch(whereCondition) {
            case 'GT':
                for (var i = 0; i < data.length; i++) {
                    if (data[i][restriction] > restrictionValue)
                        processedData.push(data[i]);
                }
                break;
            case 'LT':
                for (var i = 0; i < data.length; i++) {
                    if (data[i][restriction] < restrictionValue)
                        processedData.push(data[i]);
                }
                break;
            case 'EQ':
                for (var i = 0; i < data.length; i++) {
                    if (data[i][restriction] === restrictionValue) {
                        processedData.push(data[i]);
                    }
                }
                break;
            case 'IS':
                for (var i = 0; i < data.length; i++) {
                    if ((restrictionValue.localeCompare(data[i][restriction].toLowerCase())) == 0)
                        processedData.push(data[i]);
                }
                break;
            default:
                console.log("Attempting to process unsupported WHERE query");
                break;
        }
        return processedData
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
        var orderKey = query.ORDER;
        console.log(query.ORDER);
        var sortedData = unsortedData.sort(
            function(a,b): any {
                return a[orderKey] - b[orderKey];
            });
        console.log('we are in queryOrder')
        console.log(sortedData);
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
        let controller = QueryController.datasetController;
        console.log("hello" + JSON.stringify(query));
        // For the get query
        if (query.GET){
            console.log("inside : " + query.GET);
            queryResult = controller.queryDataset(query.GET);
            if (query.WHERE) {
                fdsa = this.queryWhere(query.WHERE, queryResult, dataset1, dataset2);
                //console.log(queryResult);
                asdf = this.queryOrder(query, fdsa);
              //  console.log(asdf);
            }
            //  testObject['result'] = queryResult;
            // testObject['render'] = 'TABLE';
            //console.log(object);
        }
        var qqqq = this.queryAs(query,asdf);
        console.log(JSON.stringify(qqqq));
        return qqqq;
    }
}
