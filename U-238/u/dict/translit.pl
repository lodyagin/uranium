#!/usr/bin/perl

$input='zali_rus_utf.txt';
$output='zali_translit.txt';

open ( IF, "$input" );
open ( OF, ">$output");

while (<IF>) {
    $a=k82tr($_);
    print OF $a;
}

close IF;
close OF;

sub k82tr
    { ($_)=@_;
    
    #
    # Fonetic correct translit
    #
    
    s/Сх/S\'h/; s/сх/s\'h/; s/СХ/S\'H/;
    s/Ш/Sh/g; s/ш/sh/g;
    
    s/Сцх/Sc\'h/; s/сцх/sc\'h/; s/СЦХ/SC\'H/;
    s/Щ/Sch/g; s/щ/sch/g;
    
    s/Цх/C\'h/; s/цх/c\'h/; s/ЦХ/C\'H/;
    s/Ч/Ch/g; s/ч/ch/g;
    
    s/Йа/J\'a/; s/йа/j\'a/; s/ЙА/J\'A/;
    s/Я/Ja/g; s/я/ja/g;
    
    s/Йо/J\'o/; s/йо/j\'o/; s/ЙО/J\'O/;
    s/Ё/Jo/g; s/ё/jo/g;
    
    s/Йу/J\'u/; s/йу/j\'u/; s/ЙУ/J\'U/;
    s/Ю/Ju/g; s/ю/ju/g;
    
    s/Э/E\'/g; s/э/e\'/g;
    s/Е/E/g; s/е/e/g;
    
    s/Зх/Z\'h/g; s/зх/z\'h/g; s/ЗХ/Z\'H/g;
    s/Ж/Zh/g; s/ж/zh/g;
    
    tr/
    абвгдзийклмнопрстуфхцъыьАБВГДЗИЙКЛМHОПРСТУФХЦЪЫЬ/
    abvgdzijklmnoprstufhc\"y\'ABVGDZIJKLMNOPRSTUFHC\"Y\'/;
    
    return $_;
    
}
    
sub tr2k8
        { ($_)=@_;
	
	#
	# Fonetic correct translit
	#
	
	s/E\'/Э/g; s/e\'/э/g;
	s/E/Е/g; s/e/е/g;
	
	s/Jo/Ё/g; s/jo/ё/g;
	s/J\'o/Йо/g; s/j\'o/йо/g; s/J\'O/ЙО/g;
	
	s/Sch/Щ/g; s/sch/щ/g;
	s/Sc\'h/Сцх/g; s/sc\'h/сцх/g; s/SC\'H/СЦХ/g;
	
	s/Ch/Ч/g; s/ch/ч/g;
	s/C\'h/Цх/g; s/c\'h/цх/g; s/C\'H/ЦХ/g;
	
	s/Sh/Ш/g; s/sh/ш/g;
	s/S\'h/Сх/g; s/s\'h/сх/g; s/S\'H/СХ/g;
	
	s/Ja/Я/g; s/ja/я/g;
	s/J\'a/Йа/g; s/j\'a/йа/g; s/J\'A/ЙА/g;
	
	s/Zh/Ж/g; s/zh/ж/g;
	s/Z\'h/Зх/g; s/z\'h/зх/g; s/Z\'H/ЗХ/g;
	
	s/Ju/Ю/g; s/ju/ю/g;
	s/J\'u/Йу/g; s/j\'u/йу/g; s/J\'U/ЙУ/g;
	
	
	tr/
	abvgdzijklmnoprstufhc\"y\'ABVGDZIJKLMNOPRSTUFHC\"Y\'/
	абвгдзийклмнопрстуфхцъыьАБВГДЗИЙКЛМHОПРСТУФХЦЪЫЬ/;
	
	return $_;
	
}

