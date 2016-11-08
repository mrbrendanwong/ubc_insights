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
import {type} from "os";

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
                    if (!fs.existsSync("data/" + id + '.json'))
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

    // Check if id belongs to a PUT resource on disk or in cache
    isValidResourceID(id: string) : boolean {
        let controller = InsightFacade.datasetController;
        if (fs.existsSync('data/' + id + '.json') || controller.getDataset(id))
            return true;
        else
            return false;
    }

    // Check if id belongs to key defined in APPLY
    isValidDefinedID(id: string, applyKeys: any[]) : boolean {
        if (applyKeys.indexOf(id) > -1)
            return true;
        else
            return false;
    }

    findInvalidKeys(invalidIDs: any[], queryKeys: any, applyKeys: any[], queryController: any) {
        for (var i = 0; i < queryKeys.length; i++) {
            var queryKey: string = queryKeys[i];
            var queryKeyID: string;

            if (queryKey.indexOf('_') != -1)
                queryKeyID = queryKey.split('_')[0];
            else
                queryKeyID = queryKey;

            if (this.isValidResourceID(queryKeyID))
                continue;
            else if (this.isValidDefinedID(queryKeyID, applyKeys))
                continue;
            else
                invalidIDs = queryController.noDuplicatePush(queryKeyID, invalidIDs);
        }
        return invalidIDs;
    }

    performQuery(query: QueryRequest): Promise<InsightResponse> {
        let that = this;
        let controller = InsightFacade.datasetController;
        return new Promise(function (fulfill, reject) {
            try {
                let datasets: Datasets = controller.getDatasets();
                let qController = new QueryController(datasets);
                let isValid = qController.isValid(query);
                let missingResource = false;

                if (isValid === true) {
                    let invalidIDs: any[] = [];
                    let getKeys = query.GET;
                    let applyKeys = qController.applyKeyExtraction(query.APPLY);
                    let whereKeys = qController.whereKeyExtraction(query.WHERE);

                    // Check if GET keys are valid
                    let getInvalidKeys = that.findInvalidKeys(invalidIDs, getKeys, applyKeys, qController);
                    invalidIDs = qController.noDuplicateConcat(getInvalidKeys, invalidIDs);

                    // Check if WHERE keys are valid
                    let whereInvalidKeys = that.findInvalidKeys(invalidIDs, whereKeys, applyKeys, qController);
                    invalidIDs = qController.noDuplicateConcat(whereInvalidKeys, invalidIDs);

                    // Do we have any missing resources?
                    if (invalidIDs.length > 0)
                        missingResource = true;

                    // Error code handling
                    if (missingResource) {
                        reject({code: 424, body: {missing: invalidIDs}});
                    } else {
                        let result = qController.query(query);
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
                reject({code: 400, body: {error: err.message}});
            }
        });
    }


}