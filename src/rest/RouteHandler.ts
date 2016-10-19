/**
 * Created by rtholmes on 2016-06-14.
 */
import restify = require('restify');
import fs = require('fs');

import DatasetController from '../controller/DatasetController';
import {Datasets} from '../controller/DatasetController';

import {QueryRequest} from "../controller/QueryController";
import Log from '../Util';
import QueryController from "../controller/QueryController";

export default class RouteHandler {

    private static datasetController = new DatasetController();

    public static getHomepage(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace('RoutHandler::getHomepage(..)');
        fs.readFile('./src/rest/views/index.html', 'utf8', function (err: Error, file: Buffer) {
            if (err) {
                res.send(500);
                Log.error(JSON.stringify(err));
                return next();
            }
            res.write(file);
            res.end();
            return next();
        });
    }

    public static  putDataset(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace('RouteHandler::postDataset(..) - params: ' + JSON.stringify(req.params));
        try {
            var id: string = req.params.id;

            // stream bytes from request into buffer and convert to base64
            // adapted from: https://github.com/restify/node-restify/issues/880#issuecomment-133485821
            let buffer: any = [];
            req.on('data', function onRequestData(chunk: any) {
                Log.trace('RouteHandler::postDataset(..) on data; chunk length: ' + chunk.length);
                buffer.push(chunk);
            });

            req.once('end', function () {
                let concated = Buffer.concat(buffer);
                req.body = concated.toString('base64');
                Log.trace('RouteHandler::postDataset(..) on end; total length: ' + req.body.length);

                let controller = RouteHandler.datasetController;
                if (controller.getDataset(id)) {
                    controller.process(id, req.body).then(function (result) {
                        Log.trace('RouteHandler::postDataset(..) - processed');
                        Log.trace("sending 201");
                        Log.trace("this is the result " + result);
                        if (result)
                            res.json(201, {success: result});
                        else
                            res.json(400, {error: "Incorrect dataset"});

                    }).catch(function (err:Error) {
                        Log.trace('RouteHandler::postDataset(..) - ERROR: ' + err.message);
                        res.json(400, {err: err.message});
                    });
                } else {
                    controller.process(id, req.body).then(function (result) {
                        Log.trace('RouteHandler::postDataset(..) - processed');
                        Log.trace("sending 204");
                        Log.trace("this is the result " + result);
                        if (result)
                            res.json(204, {success: result});
                        else
                            res.json(400, {error: "Incorrect dataset"});


                    }).catch(function (err:Error) {
                        Log.trace('RouteHandler::postDataset(..) - ERROR: ' + err.message);
                        res.json(400, {err: err.message});
                    });
                }
            });

        } catch (err) {
            Log.error('RouteHandler::postDataset(..) - ERROR: ' + err.message);
            res.send(400, {err: err.message});
        }
        return next();
    }

    // FIRST DRAFT: BRENDON DID THIS
    public static deleteDataset(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace('RouteHandler::postDataset(..) - params: ' + JSON.stringify(req.params));
        try {
            var id: string = req.params.id;

            let controller = RouteHandler.datasetController;
            if (controller.getDataset(id) || fs.existsSync("data/" + id + '.json')) {
                controller.deleteDataset(id);
                console.log('delete done in RouteHandler');
                // check if file in path has been deleted
                // if file in path deleted, res 204
                // else file in path has not been deleted for whatever reason, return an res 400 (?)
                if (fs.existsSync("data/" + id + '.json'))
                    res.json(400, {err: 'delete did not delete for some reason!'});
                else
                    res.json(204, {success: 'the operation was successful.'}) //need to fix this
            } else {
                // was not previously put, or has been deleted already, so 404
                res.json(404, {err: 'the operation was unsuccessful because the ' +
                'delete was for a resource that was not previously PUT.'});

                console.log('tripped the DELETE 404');
            }

        } catch (err) {
            Log.error('RouteHandler::deleteDataset(..) - ERROR: ' + err.message);
            res.send(400, {err: err.message});
        }
        return next();
    }

    // BRENDON DID THIS
    public static postQuery(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace('RouteHandler::postQuery(..) - params: ' + JSON.stringify(req.params));
        try {
            let query: QueryRequest = req.params;
            let dsController = RouteHandler.datasetController;
            let datasets: Datasets = dsController.getDatasets();
            let controller = new QueryController(datasets);
            let isValid = controller.isValid(query);
            let missing_resource = false;
            if (isValid === true) {
                var invalid_ids: any[] = [];
                for (var i = 0; i < query.GET.length; i++){
                    var us_index = query.GET[i].indexOf('_');
                    var query_id = query.GET[i].substring(0, us_index);

                    if (dsController.getDataset(query_id) || fs.existsSync("data/" + query_id + '.json')) {
                        continue;
                    } else {
                        console.log('RouteHandler.postQuery: substring to get id from GET keys: ' + query_id);
                        if (invalid_ids.indexOf(query_id) < 0)
                            invalid_ids[i] = query_id;
                        console.log("logged invalid ids: " + invalid_ids);
                        missing_resource = true;
                    }
                }

                if (missing_resource) {
                    res.json(424, {missing: invalid_ids});
                } else {
                    let result = controller.query(query);
                    console.log('RouteHandler.postQuery: result of controller.query(query)' + result);
                    //Brendon: unsure if "if else" required. will need QueryController.query to be completed first
                    if (result !== null){
                        res.json(200, result);
                        console.log("RouteHandler.postQuery: post query is a success!");
                    }
                    else {
                        res.json(400, {error: 'result = controller.query(query) did not return valid result?'});
                    }
                }
            } else {
                res.json(400, {error: 'invalid query. query should contain GET, WHERE, ORDER, AS'});
            }
        } catch (err) {
            Log.error('RouteHandler::postQuery(..) - ERROR: ' + err);
            res.send(403);
        }
        return next();
    }
}
