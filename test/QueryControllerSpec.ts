/**
 * Created by rtholmes on 2016-10-31.
 */

import {Datasets} from "../src/controller/DatasetController";
import QueryController from "../src/controller/QueryController";
import {QueryRequest} from "../src/controller/QueryController";
import Log from "../src/Util";

import {expect} from 'chai';
describe("QueryController", function () {

    beforeEach(function () {
    });

    afterEach(function () {
    });

    it("Should be able to validate a valid query", function () {
        // NOTE: this is not actually a valid query for D1
        let query:QueryRequest = {
            GET: ['courses_dept', 'courses_id', 'courses_instructor', 'courses_avg', 'courses_title', 'courses_pass', 'courses_fail', 'courses_audit', 'courses_uuid'],
            GROUP:['courses_dept', 'courses_id', 'courses_instructor', 'courses_avg', 'courses_title', 'courses_pass', 'courses_fail', 'courses_audit', 'courses_uuid'],
            APPLY:[],
            WHERE: {IS: {"courses_dept": "cpsc"}},
            ORDER: {"dir": "UP", "keys": ["courses_id"]},
            AS: 'table'
        };
        let dataset: Datasets = {};
        let controller = new QueryController(dataset);
        let isValid = controller.isValid(query);

        expect(isValid).to.equal(true);
    });

    //it("Should be able to validate a valid query", function () {
    //    // NOTE: this is not actually a valid query for D1
    //    let query: QueryRequest = {GET: ['courses_dept'], WHERE: {IS: {"courses_dept": 'cpsc'}}, ORDER: 'courses_dept', AS: 'table'};
    //    let dataset: Datasets = {};
    //    let controller = new QueryController(dataset);
    //    let isValid = controller.isValid(query);
    //
    //    expect(isValid).to.equal(true);
    //});


    it("Should be able to validate a valid D2 query", function () {
        let query: QueryRequest = {GET: ['courses_id', 'courseAverage'], WHERE: {IS: {"courses_dept": "cpsc"}} , GROUP: ["courses_id"], APPLY: [ {"courseAverage": {"AVG": "courses_avg"}} ], ORDER: { "dir": "UP", "keys": ["courseAverage", "courses_id"]}, AS: 'table'};
        let dataset: Datasets = {};
        let controller = new QueryController(dataset);
        let isValid = controller.isValid(query);

        expect(isValid).to.equal(true);
    });


    it("Should be able to invalidate an invalid query", function () {
        let query: any = null;
        let dataset: Datasets = {};
        let controller = new QueryController(dataset);
        let isValid = controller.isValid(query);

        expect(isValid).to.equal(false);
    });

    it("Should be able to query, although the answer will be empty", function () {
        // NOTE: this is not actually a valid query for D1, nor is the result correct.
        //let query: QueryRequest = {GET: 'food', WHERE: {IS: 'apple'}, ORDER: 'food', AS: 'table'};
        //let dataset: Datasets = {};
        //let controller = new QueryController(dataset);
        //let ret = controller.query(query);
        //Log.test('In: ' + JSON.stringify(query) + ', out: ' + JSON.stringify(ret));
        //expect(ret).not.to.be.equal(null);
        // should check that the value is meaningful
    });
});
