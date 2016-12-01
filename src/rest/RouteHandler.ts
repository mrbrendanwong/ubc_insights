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
import ScheduleController from "../controller/ScheduleController";
import InsightFacade from "../controller/InsightFacade";
import {InsightResponse} from "../controller/IInsightFacade";
export default class RouteHandler {

    private static datasetController = new DatasetController();
    private static insightFacade = new InsightFacade();
    private static scheduleController = new ScheduleController();

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
                let iController = RouteHandler.insightFacade;
                iController.addDataset(id, req.body).then(function (result) {
                    res.json(result.code, result.body);
                }).catch(function (err: InsightResponse) {
                    res.json(err.code, err.body);
                });
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
            let iController = RouteHandler.insightFacade;
            iController.removeDataset(id).then(function (result) {
                res.json(result.code,result.body);
            }).catch(function (err: InsightResponse) {
                res.json(err.code, err.body);
                console.log(res);
            });
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
            if (req.params[1] != undefined) {
                var indexString:any = Object.keys(req.params[1]);
                if (req.params[1][indexString]['seats'] != undefined){
                    let sController = RouteHandler.scheduleController;
                    //    let cController = RouteHandler.calendarController;
                    //          CalendarController.setScheduleObject(req.params);
                    //  cController.setScheduleObject(req.params);
                    sController.launchCommand();
                    return next();
                }

            }
            let iController = RouteHandler.insightFacade;
            iController.performQuery(query).then(function (result:InsightResponse) {
                res.json(result.code,result.body);
            }).catch(function (err: InsightResponse) {
                res.json(err.code, err.body);
            });
        } catch (err) {
            Log.error('RouteHandler::postQuery(..) - ERROR: ' + err);
            res.send(403);
        }
        return next();
    }
}
