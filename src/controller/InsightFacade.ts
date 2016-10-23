/**
 * Created by Spencer on 10/20/2016.
 */
import {IInsightFacade, InsightResponse} from "./IInsightFacade";
import Log from "../Util";
import DatasetController from '../controller/DatasetController';
import QueryController from "../controller/QueryController";
var fs = require("fs");
import {Datasets} from '../controller/DatasetController';

import {QueryRequest} from "../controller/QueryController";

export default class InsightFacade implements IInsightFacade {

    // TODO: need to implement this
    //private static iinsightFacade = new IInsightFacade();

    private static datasetController = new DatasetController();

    addDataset(id:string, content:any): Promise<InsightResponse>{
        let controller = InsightFacade.datasetController;
        return new Promise(function (fulfill, reject) {
            try {
                if (controller.getDataset(id)) {
                    controller.process(id, content).then(function (result) {
                        Log.trace('InsightFacade::postDataset(..) - processed');
                        Log.trace("sending 201");
                        Log.trace("this is the result " + result);
                        if (result) {
                            fulfill({code: 201, body: result});
                        }
                        else {
                            reject({code: 400, body: "Incorrect dataset"});
                        }
                    }).catch(function (err:Error) {
                        Log.trace('InsightFacade::postDataset(..) - ERROR: ' + err.message);
                        reject({code: 400, body: {error: err.message}});
                    });
                } else {
                    controller.process(id, content).then(function (result) {
                        Log.trace('InsightFacade::postDataset(..) - processed');
                        Log.trace("sending 204");
                        Log.trace("this is the result " + result);
                        if (result) {
                            fulfill({code: 204, body: result});
                        }
                        else {
                            reject({code: 400, body: "Incorrect dataset"});
                        }

                    }).catch(function (err:Error) {
                        Log.trace('InsightFacade::postDataset(..) - ERROR: ' + err.message);
                        reject({code: 400, body: {error: err.message}});
                    });
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject(err);
            }
        });
    }

    removeDataset(id:string): Promise<InsightResponse> {
        let controller = InsightFacade.datasetController;
        return new Promise(function (fulfill, reject) {
            try {
                if (controller.getDataset(id) || fs.existsSync("data/" + id + '.json')) {
                    controller.deleteDataset(id);
                    console.log('delete done in RouteHandler');
                    // check if file in path has been deleted
                    // if file in path deleted, res 204
                    // else file in path has not been deleted for whatever reason, return an res 400 (?)
                    if (fs.existsSync("data/" + id + '.json'))
                        reject({code: 404, body: 'delete did not delete for some reason!'});
                    else
                        fulfill({code: 204, body: 'the operation was successful.'});
                } else {
                    // was not previously put, or has been deleted already, so 404
                    reject({code:404, body: 'the operation was unsuccessful because the ' +
                    ' delete was for a resource that was not previously PUT.'});
                    console.log('tripped the DELETE 404');
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject(err);
            }
        });
    }

    performQuery(query: QueryRequest): Promise<InsightResponse> {
        let controller = InsightFacade.datasetController;
        return new Promise(function (fulfill, reject) {
            try {
                let dsController = InsightFacade.datasetController;
                let datasets: Datasets = dsController.getDatasets();
                let controller = new QueryController(datasets);
                let isValid = controller.isValid(query);
                let missing_resource = false;
                if (isValid === true) {
                    //        var invalid_ids: any[] = [];
                    for (var i = 0; i < query.GET.length; i++){
                        var us_index = query.GET[i].indexOf('_');
                        var query_id = query.GET[i].substring(0, us_index);

                        if (dsController.getDataset(query_id) || fs.existsSync("data/" + query_id + '.json'))
                            continue;
                        //            } else {
                        //               console.log('RouteHandler.postQuery: substring to get id from GET keys: ' + query_id);
                        //              if (invalid_ids.indexOf(query_id) < 0)
                        //                 invalid_ids[i] = query_id;
                        //            console.log("logged invalid ids: " + invalid_ids);
                        //           missing_resource = true;
                        //       }
                    }

                    //   if (missing_resource) {
                    //       reject({code: 424, body: 'missing ' + invalid_ids});
                    //   } else {
                    let result = controller.query(query);
                    console.log('RouteHandler.postQuery: result of controller.query(query)' + result);
                    //Brendon: unsure if "if else" required. will need QueryController.query to be completed first
                    if (result !== null){
                        fulfill({code: 200, body: result});
                        console.log("RouteHandler.postQuery: post query is a success!");
                    }
                    else {
                        reject({code: 400, body: 'result = controller.query(query) did not return valid result?'});
                    }
                    //   }
                } else {
                    reject({code: 400, body: 'invalid query. query should contain GET, WHERE, ORDER, AS'});
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject(err);
            }
        });
    }


}