# SnapCall

Danilo's ELM test for Snapview ELM developer position.

The solution I'm providing connects to the json-server mocked API, for getting configuration and posting commands.

# Scenarios
The solution I wrote is based on configuration and parameters, that I called scenarios. In order to
simulate situations, I've created the scenarios asked on the test assignment. To make it easier to see
the scenarions, I've added buttons on the home screen. To simulate the error, I send an invalid technology
and the API responds with an error, so I can retry.

# Acknowledgment
For each state transition I send a POST request to the API.

# How to run
In order to start the project, first install the dependencies using:
`yarn install`

then run the start command:
`yarn start`

This will start the app on `http://localhost:8080` and the json-server on `http://localhost:3000`
This will be our mock API. We use two resources: `/config` and `acknowledgments`

When you access the app address, you should see the HomePage:
![image](https://user-images.githubusercontent.com/446702/83351442-375a7480-a344-11ea-8222-f87ef8fa1615.png)

# Code quality
Run elm-analyse
`yarn analyse:watch`

# Tests
The test implementation is not complete. Due the amount of time spent on this challenge, the decision
was to prioritize the code, and add at least some tests.

`yarn test:watch`

