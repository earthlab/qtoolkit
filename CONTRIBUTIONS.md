# Contributions

## qsurvey

### Pull Request #1: [questions(design_object) returns questionType additionally](https://github.com/jamesdunham/qsurvey/pull/1)

The type of question on a qualtrics survey is included in the metadata but is not returned in a call to questions(). This pull request adds the functionality of returing the question type (ie. "MC" for Multiple Choice)

### Pull Request #2: [responses() now accepts design_object or survey ID](https://github.com/jamesdunham/qsurvey/pull/2)

Calls to `questions()`, `blocks()`, `choices()`, ... most of the api function calls accept the qsurvey design_object as the parameter, but `responses()` accepts the survey ID#. This pull requests makes `responses()` consistent with other api calls, while being backwards compatible to support the passing of the survey ID.
