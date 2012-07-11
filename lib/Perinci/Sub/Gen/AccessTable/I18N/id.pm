package Perinci::Sub::Gen::AccessTable::I18N::id;
use base 'Perinci::Sub::Gen::AccessTable::I18N';

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: Indonesian translation for Perinci::Sub::Gen::AccessTable
__DATA__

# tags

msgid  "field selection"
msgstr "pemilihan field"

msgid  "filtering"
msgstr "penyaringan"

msgid  "ordering"
msgstr "pengurutan"

msgid  "paging"
msgstr "halaman"

msgid  "filtering for %1"
msgstr "penyaringan untuk %1"

# table structure description

msgid  "Data is in table form. Table fields are as follow:"
msgstr "Data berbentuk tabel. Field-field tabel adalah sebagai berikut:"

msgid  "field"
msgstr "field"

msgid  "ID field"
msgstr "field kunci"

# arg: with_field_names

msgid  "Return field names in each record (as hash/associative array)"
msgstr "Sertakan nama field di tiap rekord (sebagai larik asosiatif/hash)"

msgid  ""
msgid  ""
"When enabled, function will return each record as hash/associative array\n"
"(field name => value pairs). Otherwise, function will return each record\n"
"as list/array (field value, field value, ...).\n"
msgstr ""
"Jika opsi ini aktif, fungsi akan mengembalikan tiap rekord sebagai larik\n"
"asosiatif/hash (pasangan nama field => nilai field). Tanpa opsi ini, fungsi\n"
"akan mengembalikan tiap rekord sebagai larik/list (nilai field, nilai ...).\n"

# arg: detail

msgid  "Return array of full records instead of just ID fields"
msgstr "Kembalikan daftar record dan tidak hanya daftar nilai field kunci"

msgid  ""
"By default, only the key (ID) field is returned per result entry.\n"
msgstr ""
"Defaultnya, hanya field kunci (ID) yang dikembalikan per entri hasil.\n"

# arg: fields

msgid  "Select fields to return"
msgstr "Pilih field yang ingin dikembalikan"

# arg: sort

msgid  "Order records according to certain field(s)"
msgstr "Urutkan rekord berdasarkan field(-field) tertentu"

msgid  ""
"A list of field names separated by comma. Each field can be prefixed with '-' to\n"
"specify descending order instead of the default ascending.\n"
msgstr ""
"Daftar nama field dipisahkan oleh koma. Setiap field dapat diawali '-' untuk\n"
"menyatakan ingin pengurutan dari besar ke kecil (default dari kecil ke besar).\n"

# arg: random

msgid  "Return records in random order"
msgstr "Kembalikan rekord dalam urutan acak"

# arg: result_limit

msgid  "Only return a certain number of records"
msgstr "Hanya kembalikan sejumlah tertentu rekord"

# arg: result_start

msgid  "Only return starting from the n'th record"
msgstr "Hanya kembalikan mulai dari rekord posisi ke-n"

# arg: q

msgid  "Search"
msgstr "Cari"

# arg: FIELD.is

msgid  "Only return records where the '%1' field equals specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya bernilai tertentu"

# arg: FIELD.has

msgid  "Only return records where the '%1' field is an array/list which contains specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya adalah larik yang mengandung nilai tertentu"

# arg: FIELD.lacks

msgid  "Only return records where the '%1' field is an array/list which does not contain specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya adalah larik yang tidak mengandung nilai tertentu"

# arg: FIELD.min

msgid  "Only return records where the '%1' field is greater than or equal to specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya lebih besar atau sama dengan nilai tertentu"

# arg: FIELD.max

msgid  "Only return records where the '%1' field is less than or equal to specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya lebih kecil atau sama dengan nilai tertentu"

# arg: FIELD.xmin

msgid  "Only return records where the '%1' field is greater than specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya lebih besar dari nilai tertentu"

# arg: FIELD.xmax

msgid  "Only return records where the '%1' field is less than specified value"
msgstr "Hanya kembalikan rekord yang field '%1'-nya lebih kecil dari nilai tertentu"

# arg: FIELD.contains

msgid  "Only return records where the '%1' field contains specified text"
msgstr "Hanya kembalikan rekord yang field '%1'-nya mengandung teks tertentu"

# arg: FIELD.not_contains

msgid  "Only return records where the '%1' field does not contain specified text"
msgstr "Hanya kembalikan rekord yang field '%1'-nya tidak mengandung teks tertentu"

# arg: FIELD.matches

msgid  "Only return records where the '%1' field matches specified regular expression pattern"
msgstr "Hanya kembalikan rekord yang field '%1'-nya cocok dengan pola regex tertentu"

# arg: FIELD.not_matches

msgid  "Only return records where the '%1' field does not match specified regular expression"
msgstr "Hanya kembalikan rekord yang field '%1'-nya tidak cocok dengan pola regex tertentu"

