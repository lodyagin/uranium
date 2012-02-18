% The source for this definitions is
% http://www.w3.org/TR/2008/REC-xml-20081126/
% Extensible Markup Language (XML) 1.0 (Fifth Edition)
%

:- module(xml_schema,
          [
           xml_name_char/1,
           xml_name_char_dom/1,
           xml_name_start_char/1,
           xml_name_start_char_dom/1,
           xml_nmtoken/1
           ]
         ).

:- use_module(library(clpfd)).

xml_name_start_char_dom(Dom) :-

  Dom =
  58 \/      % ':'
  65..90 \/  % A-Z
  95 \/      % '_'
  97..122 \/ % a-z
  0xC0..0xD6 \/
  0xD8..0xF6 \/
  0xF8..0x2FF \/
  0x370..0x37D \/
  0x37F..0x1FFF \/
  0x200C..0x200D \/
  0x2070..0x218F \/
  0x2C00..0x2FEF \/
  0x3001..0xD7FF \/
  0xF900..0xFDCF \/
  0xFDF0..0xFFFD \/
  0x10000..0xEFFFF.

xml_name_char_dom(Dom) :-

  xml_name_start_char_dom(Dom1),
  Dom = Dom1 \/
  45 \/     % '-'
  46 \/     % '.'
  48..57 \/ % 0..9
  0xB7 \/
  0x0300..0x036F \/
  0x203F..0x2040.


% 4
xml_name_start_char(C) :-

  xml_name_start_char_dom(Dom),
  C in Dom.

% 4a
xml_name_char(C) :-

  xml_name_char_dom(Dom),
  C in Dom.

% 7
xml_nmtoken([Char]) :-

  xml_name_char(Char).

xml_nmtoken([Char|Tail]) :-

  xml_name_char(Char),
  xml_nmtoken(Tail).


