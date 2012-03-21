package Perinci::Sub::Gen::AccessTable::I18N::en;
use base 'Perinci::Sub::Gen::AccessTable::I18N';

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: English translation for Perinci::Sub::Gen::AccessTable
__DATA__

msgid  "field selection"
msgstr "field selection"

# arg: show_field_names

msgid  "Show field names in result (as hash/associative array)"
msgstr "Show field names in result (as hash/associative array)"

msgid  ""
"When off, will return an array of values without field names (array/list). When\n"
"on, will return an array of field names and values (hash/associative array).\n"
msgstr ""
"When off, will return an array of values without field names (array/list). When\n"
"on, will return an array of field names and values (hash/associative array).\n"

# arg: detail

msgid  "Return detailed data (all fields)"
msgstr "Return detailed data (all fields)"

msgid  ""
"By default, only the key (ID) field is returned.\n"
msgstr ""
"By default, only the key (ID) field is returned.\n"

# arg: fields

msgid  "Select fields to return"
msgstr "Select fields to return"

msgid  "Order data according to certain field(s)"
msgstr "Order data according to certain field(s)"

msgid  ""
"A list of field names separated by comma. Each field can be prefixed with '-' to\n"
"specify descending order instead of the default ascending.\n"
msgstr ""
"A list of field names separated by comma. Each field can be prefixed with '-' to\n"
"specify descending order instead of the default ascending.\n"

