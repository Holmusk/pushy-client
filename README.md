# pushy-client

HTTP client library for [Pushy API](https://pushy.me/docs/api).

## Examples

The `/examples` directory contains a mock request made to the Pushy API. The payload of the request is a JSON object with a single field, `message`, that holds a `String` value.

To run the example, the user must have:
- A Pushy API server key
- A device token
- A Pushy client implemented on the device, that expects the payload described above.

To build and run the example executable, run the following command in the top-level directory of the project:

```sh
stack build --flag pushy-client:examples --exec example-mock-request
```
