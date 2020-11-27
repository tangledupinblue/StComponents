# St Components

Demo Fable components built using the Model -> View -> Update approach with Fable Elmish

These are simple stateless controls for use with Fable Elmish that use bootstrap 4 styling

Published under [MIT licence:](https://opensource.org/licenses/MIT)

There are likely to be breaking changes at the moment and documentation is limited due to some poor name choices as the beginning. You are welcome to copy / reference etc. If you want to use this library in actual applications please let me know so we can start to limit the breaking changes.

Brief descriptions on the components below. Example usage can be found in the example app and in ComponentDemo.fs

## Navbar
Very simple Navbar

## Helpers
These are simple controls for easily adding different types of help contexts to a form / application including pulling in html from a different url to display in a modal dialog

## FormHolders
The Form Holder is a collection of Field Holders that hold values for individual fields. The updates are channeled through one update Msg so that they can be easily added to the calling application.

This allows you to create forms with different control types with minimal plumbing.

There are a number of different types of fields, behaviour such as ReadOnly can be defined, there are some layout options for lists and options to specify help messages.

Form validations can be set as well as refresh actions for conditional updating of field(s) based on changes to a specific field.

Forms can also be mapped to types or json using Thoth.Json

## QueryData
This is a "table" that can be serialized/deserialized with json for server queries. It is used as the underlying data structure for the DataGrid

## DataGrid
This is a simple data grid that allows for sorting / filtering / basic styling and clickable events that can be handled on the calling function such as row click / row double click etc.

This is in the demo under Grid

## Report
The report uses the DataGrid to display data but adds simple column grouping and aggregating and the option to show / hide details

This is in the demo under Report















