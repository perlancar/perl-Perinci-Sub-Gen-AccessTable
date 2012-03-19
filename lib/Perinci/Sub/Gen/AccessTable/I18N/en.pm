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

'Show field names in result (as hash/associative array)'

msgid  ""
"When off, will return an array of values without field names (array/list). When\n"
"on, will return an array of field names and values (hash/associative array).\n"
msgstr ""
"When off, will return an array of values without field names (array/list). When\n"
"on, will return an array of field names and values (hash/associative array).\n"

