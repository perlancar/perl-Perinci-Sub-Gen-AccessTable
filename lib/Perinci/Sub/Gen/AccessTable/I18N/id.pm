package Perinci::Sub::Gen::AccessTable::I18N::id;
use base 'Perinci::Sub::Gen::AccessTable::I18N';

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: Indonesian translation for Perinci::Sub::Gen::AccessTable
__DATA__

msgid  "field selection"
msgstr "pemilihan field"

# arg: show_field_names

msgid  "Show field names in result (as hash/associative array)"
msgstr "Tampilkan nama field di hasil (sebagai larik asosiatif/hash)"

msgid  ""
"When off, will return an array of values without field names (array/list). When\n"
"on, will return an array of field names and values (hash/associative array).\n"
msgstr ""
"Jika nonaktif, akan mengembalikan larik nilai tanpa nama field. Jika aktif,\n"
"mengembalikan larik asosiatif (hash) berisi nama field nilai field.\n"

# arg: detail

msgid  "Return detailed data (all fields)"
msgstr "Kembalikan hasil detil (semua field)"

msgid  ""
"By default, only the key (ID) field is returned.\n"
msgstr ""
"Defaultnya, hanya field kunci (ID) yang dikembalikan.\n"

