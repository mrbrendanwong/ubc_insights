/**
 * Created by Spencer on 10/20/2016.
 */

import fs = require('fs');
import Log from "../src/Util";
import {expect} from 'chai';
import InsightFacade from "../src/controller/InsightFacade";
import {InsightResponse} from "../src/controller/IInsightFacade";
import {QueryRequest} from "../src/controller/QueryController";

describe("InsightFacade", function () {

    var zipFileContents:string = null;
    var facade:InsightFacade = null;
    before(function () {
        Log.info('InsightController::before() - start');
        // this zip might be in a different spot for you
        zipFileContents = new Buffer(fs.readFileSync('../cpsc310d1public/310courses.1.0.zip')).toString('base64');
        try {
            // what you delete here is going to depend on your impl, just make sure
            // all of your temporary files and directories are deleted
            fs.unlinkSync('./id.json');
        } catch (err) {
            // silently fail, but don't crash; this is fine
            Log.warn('InsightController::before() - id.json not removed (probably not present)');
        }
        Log.info('InsightController::before() - done');
    });

    beforeEach(function () {
        facade = new InsightFacade();
    });

    it("Should be able to add a add a new dataset (204)", function () {
        var that = this;
        Log.trace("Starting test: " + that.test.title);
        return facade.addDataset('courses', zipFileContents).then(function (response:InsightResponse) {
            expect(response.code).to.equal(204);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });
    });

    it("Should be able to update an existing dataset (201)", function () {
        var that = this;
        Log.trace("Starting test: " + that.test.title);
        return facade.addDataset('courses', zipFileContents).then(function (response:InsightResponse) {
            expect(response.code).to.equal(201);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });
    });

    it("Should not be able to add an invalid dataset (400)", function () {
        var that = this;
        Log.trace("Starting test: " + that.test.title);
        return facade.addDataset('courses', 'some random bytes').then(function (response:InsightResponse) {
            expect.fail();
        }).catch(function (response:InsightResponse) {
            expect(response.code).to.equal(400);
        });
    });

    it("Should be able to handle simple query", function () {
        var that = this;
        let query:QueryRequest = {
            GET: ['courses_id', 'courseAverage'],
            WHERE: {IS: {"courses_dept": "cpsc"}},
            GROUP: ["courses_id"],
            APPLY: [{"courseAverage": {"AVG": "courses_avg"}}],
            ORDER: {"dir": "UP", "keys": ["courseAverage", "courses_id"]},
            AS: 'table'
        };

        Log.trace("Starting test: " + that.test.title);
        return facade.performQuery(query).then(function (response:InsightResponse) {
            expect(response.code).to.equal(200);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });

    });


    it("Should be handle retrieving all IDs", function () {
        var that = this;
        let query:QueryRequest = {
            GET: ['courses_dept', 'courses_id', 'courses_instructor', 'courses_avg', 'courses_title', 'courses_pass', 'courses_fail', 'courses_audit', 'courses_uuid'],
            GROUP:['courses_dept', 'courses_id', 'courses_instructor', 'courses_avg', 'courses_title', 'courses_pass', 'courses_fail', 'courses_audit', 'courses_uuid'],
            APPLY:[],
            WHERE: {IS: {"courses_dept": "cpsc"}},
            ORDER: {"dir": "UP", "keys": ["courses_id"]},
            AS: 'table'
        };

        Log.trace("Starting test: " + that.test.title);
        return facade.performQuery(query).then(function (response:InsightResponse) {
            expect(response.code).to.equal(200);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });

    });

    // Delete Dataset Tests
    it("Should be able to delete existing dataset (204)", function () {
        var that = this;
        Log.trace("Starting test: " + that.test.title);
        facade.addDataset('courses', zipFileContents)
        return facade.removeDataset('courses').then(function (response: InsightResponse) {
            expect(response.code).to.equal(204);
        }).catch(function (response: InsightResponse) {
            expect.fail('Should not happen');
        });
    });

    it("Should not be able to delete non-existing dataset (404)", function () {
        var that = this;
        Log.trace("Starting test: " + that.test.title);
        facade.addDataset('courses', zipFileContents)
        return facade.removeDataset('courses').then(function (response: InsightResponse) {
            expect.fail();
        }).catch(function (response: InsightResponse) {
            expect(response.code).to.equal(404);
        });
    });

    it("Should not be able to query with invalid IDs (424)", function () {
        var that = this;
        let query:QueryRequest =   {
            "GET": ["courses_id", "courseAverage", "kombucha_avg"],
            "WHERE": {"IS": {"courses_dept": "cpsc"}} ,
            "GROUP": [ "courses_id", "kombucha_avg"],
            "APPLY": [ {"courseAverage": {"AVG": "courses_avg"}} ],
            "ORDER": { "dir": "UP", "keys": ["courseAverage", "courses_id"]},
            "AS":"TABLE"
        };
        return facade.performQuery(query).then(function (response:InsightResponse) {
            expect.fail();
        }).catch(function (response: InsightResponse) {
            expect(response.code).to.equal(424);
        });
    });


    it("Should be able to handle complex D1 query", function () {
        var that = this;
        let query:QueryRequest = {
            GET: ["courses_dept", "courses_id", "courses_instructor"], WHERE: {
                "OR": [
                    {
                        "AND": [
                            {"GT": {"courses_avg": 70}},
                            {"IS": {"courses_dept": "cp*"}},
                            {"NOT": {"IS": {"courses_instructor": "murphy, gail"}}}
                        ]
                    },
                    {"IS": {"courses_instructor": "*gregor*"}}
                ]
            },
            GROUP: [ "courses_dept", "courses_id", "courses_instructor"],
            APPLY: [],
            ORDER: { "dir": "UP", "keys": ["courses_dept"]},
            AS: "TABLE"
        };

        Log.trace("Starting test: " + that.test.title);
        return facade.performQuery(query).then(function (response:InsightResponse) {
            console.log("HOWDY "+ JSON.stringify(response));
            expect(response.code).to.equal(200);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });

    });



    it("Should be able to handle another complex D1 query", function () {
        var that = this;
        let query:QueryRequest = {
            GET: ["courses_dept", "courses_id", "courses_instructor"], WHERE: {
                "OR": [
                    {
                        "AND": [
                            {"LT": {"courses_avg": 70}},
                            {"IS": {"courses_dept": "cp*"}},
                            {"NOT": {"IS": {"courses_instructor": "murphy, gail"}}}
                        ]
                    },
                    {"EQ": {"courses_avg": 95}}
                ]

            },
            GROUP: [ "courses_dept", "courses_id", "courses_instructor"],
            APPLY: [],
            ORDER: { "dir": "UP", "keys": ["courses_dept"]},
            AS: "TABLE"
        };

        Log.trace("Starting test: " + that.test.title);
        return facade.performQuery(query).then(function (response:InsightResponse) {
            expect(response.code).to.equal(200);
        }).catch(function (response:InsightResponse) {
            expect.fail('Should not happen');
        });
    });

    it("Should be able to handle another complex D2 query", function () {
        var that = this;
        var query = { GET: ['courses_id', 'numSections', 'maxFail', 'minPass', "courses_dept"], WHERE: { IS: { "courses_dept": "cpsc" } }, GROUP: [ "courses_dept", "courses_id" ], APPLY: [ {"numSections": {"COUNT": "courses_uuid"}}, {"maxFail": {"MAX": "courses_fail"}}, {"minPass": {"MIN": "courses_pass"}} ], ORDER:  { "dir": "UP", "keys": ["numSections", "courses_dept", "courses_id"]}, AS: 'table' };
        Log.trace("Starting test: " + that.test.title);
        return facade.performQuery(query).then(function (response) {
            expect(response.code).to.equal(200);
        }).catch(function (response) {
            expect.fail('Should not happen');
        });
    });

    it("Should be equivalent to D2 query result", function () {
        var that = this;
        let query: QueryRequest = {GET: ['courses_id', 'courseAverage'], WHERE: {IS: {"courses_dept": "cpsc"}} , GROUP: ["courses_id"], APPLY: [ {"courseAverage": {"AVG": "courses_avg"}} ], ORDER: { "dir": "UP", "keys": ["courseAverage", "courses_id"]}, AS: 'TABLE'};
        return facade.performQuery(query).then(function (response:InsightResponse) {
            expect(response.code).to.equal(200);
        }).catch(function (response) {
            expect.fail("Should not occur");
        })
    })
});