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
            console.log('QueryController.isValid: Object.keys(query) is ' + Object.keys(query));
            return true;
        }
        return false;
    }

    public query(query: QueryRequest): QueryResponse {
        Log.trace('QueryController::query( ' + JSON.stringify(query) + ' )');

        // TODO: implement this (where we handle get, where, etc.)

        var resp: QueryResponse;
        let controller = QueryController.datasetController;
        console.log("hello" + JSON.stringify(query));
        // For the get query
        if (query.GET){
            console.log("inside : " + query.GET);
            for (var i =0; i < query.GET.length; i++){
                switch (query.GET[i]) {
                    case 'courses_dept':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_id':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_avg':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_instructor':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_title':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_pass':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_fail':
                        controller.queryDataset(query.GET[i]);
                        break;
                    case 'courses_audit':
                        controller.queryDataset(query.GET[i]);
                        break;
                    default:
                        console.log("Uh oh, you sent an invalid key");
                        break;
                }
            }
        }
        //if (query.add) {
        //    let res = 0;
        //    for(var n of query.add) {
        //        res += n;
        //    }
        //    resp.result = res;
        //} else if (query.multiply) {
        //    let res = 1;
        //    for(var n of query.add) {
        //        res += n;
        //    }
        //    resp.result = res;
        //
        //} else {
        //    resp.error = 'bad name';
        //}
        //return {status: 'received', ts: new Date().getTime()};
        return {result: 123};
    }
}
