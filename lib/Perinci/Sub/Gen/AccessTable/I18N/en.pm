package Perinci::Sub::Gen::AccessTable::I18N::en;
use base 'Perinci::Sub::Gen::AccessTable::I18N';

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: English translation for Perinci::Sub::Gen::AccessTable
__DATA__

# tags

msgid  "field selection"
msgstr "field selection"

msgid  "filtering"
msgstr "filtering"

msgid  "ordering"
msgstr "ordering"

msgid  "paging"
msgstr "paging"

msgid  "filtering for %1"
msgstr "filtering for %1"

# table structure description

msgid  "Data is in table form. Table fields are as follow:"
msgstr "Data is in table form. Table fields are as follow:"

msgid  "field"
msgstr "field"

msgid  "ID field"
msgstr "ID field"

# arg: with_field_names

msgid  "Return field names in each record (as hash/associative array)"
msgstr "Return field names in each record (as hash/associative array)"

msgid  ""
"When enabled, function will return each record as hash/associative array\n"
"(field name => value pairs). Otherwise, function will return each record\n"
"as list/array (field value, field value, ...).\n"
msgstr ""
"When enabled, function will return each record as hash/associative array\n"
"(field name => value pairs). Otherwise, function will return each record\n"
"as list/array (field value, field value, ...).\n"

# arg: detail

msgid  "Return array of full records instead of just ID fields"
msgstr "Return array of full records instead of just ID fields"

msgid  ""
"By default, only the key (ID) field is returned per result entry.\n"
msgstr ""
"By default, only the key (ID) field is returned per result entry.\n"

# arg: fields

msgid  "Select fields to return"
msgstr "Select fields to return"

# arg: sort

msgid  "Order records according to certain field(s)"
msgstr "Order records according to certain field(s)"

msgid  ""
"A list of field names separated by comma. Each field can be prefixed with '-' to\n"
"specify descending order instead of the default ascending.\n"
msgstr ""
"A list of field names separated by comma. Each field can be prefixed with '-' to\n"
"specify descending order instead of the default ascending.\n"

# arg: random

msgid  "Return records in random order"
msgstr "Return records in random order"

# arg: result_limit

msgid  "Only return a certain number of records"
msgstr "Only return a certain number of records"

# arg: result_start

msgid  "Only return starting from the n'th record"
msgstr "Only return starting from the n'th record"

# arg: q

msgid  "Search"
msgstr "Search"

# arg: FIELD.is

msgid  "Only return records where the '%1' field equals specified value"
msgstr "Only return records where the '%1' field equals specified value"

# arg: FIELD.isnt

msgid  "Only return records where the '%1' field does not equal specified value"
msgstr "Only return records where the '%1' field does not equal specified value"

# arg: FIELD.in

msgid  "Only return records where the '%1' field is in the specified values"
msgstr "Only return records where the '%1' field is in the specified values"

# arg: FIELD.not_in

msgid  "Only return records where the '%1' field is not in the specified values"
msgstr "Only return records where the '%1' field is not in the specified values"

# arg: FIELD.has

msgid  "Only return records where the '%1' field is an array/list which contains specified value"
msgstr "Only return records where the '%1' field is an array/list which contains specified value"

# arg: FIELD.lacks

msgid  "Only return records where the '%1' field is an array/list which does not contain specified value"
msgstr "Only return records where the '%1' field is an array/list which does not contain specified value"

# arg: FIELD.min

msgid  "Only return records where the '%1' field is greater than or equal to specified value"
msgstr "Only return records where the '%1' field is greater than or equal to specified value"

# arg: FIELD.max

msgid  "Only return records where the '%1' field is less than or equal to specified value"
msgstr "Only return records where the '%1' field is less than or equal to specified value"

# arg: FIELD.xmin

msgid  "Only return records where the '%1' field is greater than specified value"
msgstr "Only return records where the '%1' field is greater than specified value"

# arg: FIELD.xmax

msgid  "Only return records where the '%1' field is less than specified value"
msgstr "Only return records where the '%1' field is less than specified value"

# arg: FIELD.contains

msgid  "Only return records where the '%1' field contains specified text"
msgstr "Only return records where the '%1' field contains specified text"

# arg: FIELD.not_contains

msgid  "Only return records where the '%1' field does not contain specified text"
msgstr "Only return records where the '%1' field does not contain a certain text"

# arg: FIELD.matches

msgid  "Only return records where the '%1' field matches specified regular expression pattern"
msgstr "Only return records where the '%1' field matches specified regular expression pattern"

# arg: FIELD.not_matches

msgid  "Only return records where the '%1' field does not match specified regular expression"
msgstr "Only return records where the '%1' field does not match specified regular expression"
