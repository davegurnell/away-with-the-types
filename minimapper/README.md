First, ADTs give us
a reliable way of traversing
the type hierarchy in our code.

The general term for this is "structural recursion",
which we can represent in Scala in two ways...


by method inheritance and by pattern matching.





## Notes for start of talk

Describe product and sum types in more detail.

## Define Data ADT

Simple ADT, similar to a JSON ADT.
We'll define a couple of extras
to make our lives easier:

- Direct support for timestamps
  (the real Cartographer data language
  has a couple more special cases)

- Direct representations
  of product and sum types

## Define Schema ADT

Our data ADT provides a substitute
for instances of our Survey ADT.

However, we lose a chunk of type safety
by making the whole thing generic.
We know that surveys are always
some combination of products and sums
of a set of core field types,
but we don't know what fields should be what types.

We can get around this by creating schema objects
to replace the type definitions for our surveys.
We'll add a couple of simple conveniences, too,
like a "default value" schema
to replace the default field values in a case class.

## Define `empty` and `typeCheck` Functions on Schema

We can implement quite a lot of functionality
by simply walking the schema.

For example, we can define an `empty` method
to produce a blank survey to be filled in by the user.

Type checking is a similar process.
We walk the schema and data together,
checking each field.
We return a list of errors.
Cartographer's typeCheck function
reports the locations of the type errors
along-side error messages.
Minimapper simplifies this to just the error messages.

## Define `ToData` and `FromData` type classes

To prevent bugs, we want to check that our
new schemas represent the same data types
as our old Survey ADT.

We do this with property-based testing
and two type classes: `ToData` and `FromData`.

Demonstrate the type classes.
Show their implementation.
Mention the shapeless parts,
refer to shapeless-guide and last year's talk.

## Define JSON Codecs

Encoder[Data] is just a type class.

Decoding JSON is ambiguous.
We'd normally disambiguate by specifying a target type:

Decoder[MyType].decode(someJson)

However, we don't have a type any more!
We have to generate the decoder from the schema.

## Working with Schemas

- Expressions
- Lenses/Cursors
- Use of ToData and FromData

# Next Steps

Scaffolding web forms.
We can use our schema objects to generate
vanilla web forms for editing.
However, to create a decent UI
we need more information:

- labels for the form fields;
- help text;
- customising certain widgets etc.

For this reason, Cartographer uses
a third ADT representing a form.
This is type checked against the schema
using a simple traversal.

# Further Reading