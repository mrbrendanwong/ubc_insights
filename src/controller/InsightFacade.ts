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
                            reject({code: 400, body: {error: "Incorrect dataset"}});
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
                            reject({code: 400, body: {error: "Incorrect dataset"}});
                        }

                    }).catch(function (err:Error) {
                        Log.trace('InsightFacade::postDataset(..) - ERROR: ' + err.message);
                        reject({code: 400, body: {error: err.message}});
                    });
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject({code: 400, body: {error: err.message}});
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
                    if (fs.existsSync("data/" + id + '.json'))
                        reject({code: 400, body: {error: 'delete did not delete for some reason!'}});
                    else
                        fulfill({code: 204, body: 'the operation was successful.'});
                } else {
                    reject({code:404, body: {error: 'the operation was unsuccessful because the ' +
                    'delete was for a resource that was not previously PUT.'}});
                    console.log('tripped the DELETE 404');
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject({code: 400, body: {error: err.message}});
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
                    var invalid_ids: any[] = [];

                    if (query.APPLY != (undefined || null)) {
                        var applyKeys: any[] = [];
                        for (var j = 0; j < query.APPLY.length; j++) {
                            console.log(Object.keys(query.APPLY[j])[0]);
                            applyKeys[j] = Object.keys(query.APPLY[j])[0];
                        }
                    }

                    for (var i = 0; i < query.GET.length; i++){
                        var getDatasetID: string = query.GET[i];
                        if (getDatasetID.indexOf('_') != -1)
                            getDatasetID = getDatasetID.split('_')[0];
                        if (dsController.getDataset(getDatasetID)
                            || fs.existsSync("data/" + getDatasetID + '.json')
                            || query.GROUP.indexOf(getDatasetID) > -1
                            || applyKeys.indexOf(getDatasetID) > -1) {
                            continue;
                        } else {
                            console.log('RouteHandler.postQuery: substring to get id from GET keys: ' + getDatasetID);
                            if (invalid_ids.indexOf(getDatasetID) < 0)
                                invalid_ids.push(getDatasetID);
                            console.log("logged invalid ids: " + invalid_ids);
                            missing_resource = true;
                        }
                    }

                    //if (query.GROUP != (undefined || null)) {
                    //    for (var k = 0; k < query.GROUP.length; k++){
                    //        console.log("In InsightFacade" + query.GROUP[k]);
                    //        var groupDatasetID: string = query.GROUP[k];
                    //        if (query.GET.indexOf(groupDatasetID) > -1)
                    //            continue;
                    //        else {
                    //            if (invalid_ids.indexOf(groupDatasetID) < 0) {
                    //                if (groupDatasetID.indexOf('_') !== -1)
                    //                    groupDatasetID = groupDatasetID.split('_')[0];
                    //                invalid_ids.push(groupDatasetID);
                    //            }
                    //            missing_resource = true;
                    //        }
                    //    }
                    //}

                    if (missing_resource) {
                        reject({code: 424, body: {missing: invalid_ids}});
                    } else {
                        let result = controller.query(query);
                        console.log('RouteHandler.postQuery: result of controller.query(query)' + result);
                        if (result !== null){
                            fulfill({code: 200, body: result});
                            console.log("RouteHandler.postQuery: post query is a success!");
                        } else {
                            reject({code: 400,
                                body: {error: 'result = controller.query(query) did not return valid result?'}});
                        }
                    }
                } else {
                    reject({code: 400, body: {error: 'invalid query. please fix query formatting'}});
                }
            } catch (err) {
                Log.trace('DatasetController::process(..) - ERROR: ' + err);
                reject({code: 400,  body: {error: 'invalid query. please fix query formatting'}});
            }
        });
    }


}