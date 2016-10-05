/**
 * Created by rtholmes on 2016-06-19.
 */

import {Datasets} from "./DatasetController";
import Log from "../Util";

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

        var resp: QueryResponse;

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
