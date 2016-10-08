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
    result?: number;
}

export default class QueryController {
    private static datasetController = new DatasetController();
    private datasets: Datasets = null;

    constructor(datasets: Datasets) {
        this.datasets = datasets;
    }

    public isValid(query: QueryRequest): boolean {
        if (typeof query !== 'undefined' && query !== null && Object.keys(query).length > 0) {
            return true;
        }
        return false;
    }

    public query(query: QueryRequest): QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');

        // TODO: implement this (where we handle get, where, etc.)
        let testObject = {};
        var resp: QueryResponse;
        let queryResult:Array<any>;
        let controller = QueryController.datasetController;
        console.log("hello" + JSON.stringify(query));
        // For the get query
        if (query.GET){
            console.log("inside : " + query.GET);
            queryResult = controller.queryDataset(query.GET);
            let object = {};

          //  testObject['result'] = queryResult;
           // testObject['render'] = 'TABLE';
            //console.log(object);
        }
        return {result: queryResult};
    }
}
