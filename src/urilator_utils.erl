-module(urilator_utils).


-export([pipe/2]).
-export([split/2, split/3]).
-export([quote/1, unquote/1]).


-type pipe_func() :: fun((term()) -> {ok, term()} | {error, term()}).


-spec pipe(FuncList, State) -> {ok, NewState} | {error, Label, Reason, NewState} when
      FuncList :: [{Label, pipe_func()}],
      State    :: term(),
      NewState :: term(),
      Reason   :: term(),
      Label    :: atom().
pipe([], State) ->
    {ok, State};
pipe([{Label, Func}|Rest], State) ->
    case Func(State) of
        {ok, NewState}            -> pipe(Rest, NewState);
        {error, Reason}           -> {error, Label, Reason, State};
        {error, Reason, NewState} -> {error, Label, Reason, NewState}
    end.



-spec split(Binary, Separator) -> Result when
      Binary    :: binary(),
      Separator :: binary(),
      Result    :: [binary()].
split(Binary, Separator) ->
    split(Binary, Separator, -1).



-spec split(Binary, Separator, Times) -> Result when
      Binary    :: binary(),
      Separator :: binary(),
      Times     :: integer(),
      Result    :: [binary()].
split(Binary, Separator, Times) ->
    split(Binary, Separator, size(Separator), Times, <<>>, []).


-spec split(Binary, Separator, SeparatorSize, Times, Buf, Acc) -> Result when
      Binary        :: binary(),
      Separator     :: binary(),
      SeparatorSize :: non_neg_integer(),
      Times         :: integer(),
      Buf           :: binary(),
      Acc           :: [binary()],
      Result        :: [binary()].
split(Bin, _Separator, _SeparatorSize, Times, _Buf, Acc) when Times == 0 ->
    lists:reverse([Bin|Acc]);
split(<<>>, _Separator, _SeparatorSize, _Times, Buf, Acc) ->
    lists:reverse([Buf|Acc]);
split(Bin, Separator, SeparatorSize, Times, Buf, Acc) ->
    {Rest, NewTimes, NewBuf, NewAcc} = case Bin of
        <<Separator:SeparatorSize/binary, R/binary>> -> {R, Times - 1, <<>>, [Buf|Acc]};
        <<Char:1/binary, R/binary>>                  -> {R, Times, <<Buf/binary, Char:1/binary>>, Acc}
    end,
    split(Rest, Separator, SeparatorSize, NewTimes, NewBuf, NewAcc).


-spec quote(char()) -> binary().
quote(0)   -> <<"%00">>;
quote(1)   -> <<"%01">>;
quote(2)   -> <<"%02">>;
quote(3)   -> <<"%03">>;
quote(4)   -> <<"%04">>;
quote(5)   -> <<"%05">>;
quote(6)   -> <<"%06">>;
quote(7)   -> <<"%07">>;
quote(8)   -> <<"%08">>;
quote(9)   -> <<"%09">>;
quote(10)  -> <<"%0A">>;
quote(11)  -> <<"%0B">>;
quote(12)  -> <<"%0C">>;
quote(13)  -> <<"%0D">>;
quote(14)  -> <<"%0E">>;
quote(15)  -> <<"%0F">>;
quote(16)  -> <<"%10">>;
quote(17)  -> <<"%11">>;
quote(18)  -> <<"%12">>;
quote(19)  -> <<"%13">>;
quote(20)  -> <<"%14">>;
quote(21)  -> <<"%15">>;
quote(22)  -> <<"%16">>;
quote(23)  -> <<"%17">>;
quote(24)  -> <<"%18">>;
quote(25)  -> <<"%19">>;
quote(26)  -> <<"%1A">>;
quote(27)  -> <<"%1B">>;
quote(28)  -> <<"%1C">>;
quote(29)  -> <<"%1D">>;
quote(30)  -> <<"%1E">>;
quote(31)  -> <<"%1F">>;
quote(32)  -> <<"%20">>;
quote(33)  -> <<"%21">>;
quote(34)  -> <<"%22">>;
quote(35)  -> <<"%23">>;
quote(36)  -> <<"%24">>;
quote(37)  -> <<"%25">>;
quote(38)  -> <<"%26">>;
quote(39)  -> <<"%27">>;
quote(40)  -> <<"%28">>;
quote(41)  -> <<"%29">>;
quote(42)  -> <<"%2A">>;
quote(43)  -> <<"%2B">>;
quote(44)  -> <<"%2C">>;
quote(45)  -> <<"-">>;
quote(46)  -> <<".">>;
quote(47)  -> <<"%2F">>;
quote(48)  -> <<"0">>;
quote(49)  -> <<"1">>;
quote(50)  -> <<"2">>;
quote(51)  -> <<"3">>;
quote(52)  -> <<"4">>;
quote(53)  -> <<"5">>;
quote(54)  -> <<"6">>;
quote(55)  -> <<"7">>;
quote(56)  -> <<"8">>;
quote(57)  -> <<"9">>;
quote(58)  -> <<"%3A">>;
quote(59)  -> <<"%3B">>;
quote(60)  -> <<"%3C">>;
quote(61)  -> <<"%3D">>;
quote(62)  -> <<"%3E">>;
quote(63)  -> <<"%3F">>;
quote(64)  -> <<"%40">>;
quote(65)  -> <<"A">>;
quote(66)  -> <<"B">>;
quote(67)  -> <<"C">>;
quote(68)  -> <<"D">>;
quote(69)  -> <<"E">>;
quote(70)  -> <<"F">>;
quote(71)  -> <<"G">>;
quote(72)  -> <<"H">>;
quote(73)  -> <<"I">>;
quote(74)  -> <<"J">>;
quote(75)  -> <<"K">>;
quote(76)  -> <<"L">>;
quote(77)  -> <<"M">>;
quote(78)  -> <<"N">>;
quote(79)  -> <<"O">>;
quote(80)  -> <<"P">>;
quote(81)  -> <<"Q">>;
quote(82)  -> <<"R">>;
quote(83)  -> <<"S">>;
quote(84)  -> <<"T">>;
quote(85)  -> <<"U">>;
quote(86)  -> <<"V">>;
quote(87)  -> <<"W">>;
quote(88)  -> <<"X">>;
quote(89)  -> <<"Y">>;
quote(90)  -> <<"Z">>;
quote(91)  -> <<"%5B">>;
quote(92)  -> <<"%5C">>;
quote(93)  -> <<"%5D">>;
quote(94)  -> <<"%5E">>;
quote(95)  -> <<"_">>;
quote(96)  -> <<"%60">>;
quote(97)  -> <<"a">>;
quote(98)  -> <<"b">>;
quote(99)  -> <<"c">>;
quote(100) -> <<"d">>;
quote(101) -> <<"e">>;
quote(102) -> <<"f">>;
quote(103) -> <<"g">>;
quote(104) -> <<"h">>;
quote(105) -> <<"i">>;
quote(106) -> <<"j">>;
quote(107) -> <<"k">>;
quote(108) -> <<"l">>;
quote(109) -> <<"m">>;
quote(110) -> <<"n">>;
quote(111) -> <<"o">>;
quote(112) -> <<"p">>;
quote(113) -> <<"q">>;
quote(114) -> <<"r">>;
quote(115) -> <<"s">>;
quote(116) -> <<"t">>;
quote(117) -> <<"u">>;
quote(118) -> <<"v">>;
quote(119) -> <<"w">>;
quote(120) -> <<"x">>;
quote(121) -> <<"y">>;
quote(122) -> <<"z">>;
quote(123) -> <<"%7B">>;
quote(124) -> <<"%7C">>;
quote(125) -> <<"%7D">>;
quote(126) -> <<"%7E">>;
quote(127) -> <<"%7F">>;
quote(128) -> <<"%80">>;
quote(129) -> <<"%81">>;
quote(130) -> <<"%82">>;
quote(131) -> <<"%83">>;
quote(132) -> <<"%84">>;
quote(133) -> <<"%85">>;
quote(134) -> <<"%86">>;
quote(135) -> <<"%87">>;
quote(136) -> <<"%88">>;
quote(137) -> <<"%89">>;
quote(138) -> <<"%8A">>;
quote(139) -> <<"%8B">>;
quote(140) -> <<"%8C">>;
quote(141) -> <<"%8D">>;
quote(142) -> <<"%8E">>;
quote(143) -> <<"%8F">>;
quote(144) -> <<"%90">>;
quote(145) -> <<"%91">>;
quote(146) -> <<"%92">>;
quote(147) -> <<"%93">>;
quote(148) -> <<"%94">>;
quote(149) -> <<"%95">>;
quote(150) -> <<"%96">>;
quote(151) -> <<"%97">>;
quote(152) -> <<"%98">>;
quote(153) -> <<"%99">>;
quote(154) -> <<"%9A">>;
quote(155) -> <<"%9B">>;
quote(156) -> <<"%9C">>;
quote(157) -> <<"%9D">>;
quote(158) -> <<"%9E">>;
quote(159) -> <<"%9F">>;
quote(160) -> <<"%A0">>;
quote(161) -> <<"%A1">>;
quote(162) -> <<"%A2">>;
quote(163) -> <<"%A3">>;
quote(164) -> <<"%A4">>;
quote(165) -> <<"%A5">>;
quote(166) -> <<"%A6">>;
quote(167) -> <<"%A7">>;
quote(168) -> <<"%A8">>;
quote(169) -> <<"%A9">>;
quote(170) -> <<"%AA">>;
quote(171) -> <<"%AB">>;
quote(172) -> <<"%AC">>;
quote(173) -> <<"%AD">>;
quote(174) -> <<"%AE">>;
quote(175) -> <<"%AF">>;
quote(176) -> <<"%B0">>;
quote(177) -> <<"%B1">>;
quote(178) -> <<"%B2">>;
quote(179) -> <<"%B3">>;
quote(180) -> <<"%B4">>;
quote(181) -> <<"%B5">>;
quote(182) -> <<"%B6">>;
quote(183) -> <<"%B7">>;
quote(184) -> <<"%B8">>;
quote(185) -> <<"%B9">>;
quote(186) -> <<"%BA">>;
quote(187) -> <<"%BB">>;
quote(188) -> <<"%BC">>;
quote(189) -> <<"%BD">>;
quote(190) -> <<"%BE">>;
quote(191) -> <<"%BF">>;
quote(192) -> <<"%C0">>;
quote(193) -> <<"%C1">>;
quote(194) -> <<"%C2">>;
quote(195) -> <<"%C3">>;
quote(196) -> <<"%C4">>;
quote(197) -> <<"%C5">>;
quote(198) -> <<"%C6">>;
quote(199) -> <<"%C7">>;
quote(200) -> <<"%C8">>;
quote(201) -> <<"%C9">>;
quote(202) -> <<"%CA">>;
quote(203) -> <<"%CB">>;
quote(204) -> <<"%CC">>;
quote(205) -> <<"%CD">>;
quote(206) -> <<"%CE">>;
quote(207) -> <<"%CF">>;
quote(208) -> <<"%D0">>;
quote(209) -> <<"%D1">>;
quote(210) -> <<"%D2">>;
quote(211) -> <<"%D3">>;
quote(212) -> <<"%D4">>;
quote(213) -> <<"%D5">>;
quote(214) -> <<"%D6">>;
quote(215) -> <<"%D7">>;
quote(216) -> <<"%D8">>;
quote(217) -> <<"%D9">>;
quote(218) -> <<"%DA">>;
quote(219) -> <<"%DB">>;
quote(220) -> <<"%DC">>;
quote(221) -> <<"%DD">>;
quote(222) -> <<"%DE">>;
quote(223) -> <<"%DF">>;
quote(224) -> <<"%E0">>;
quote(225) -> <<"%E1">>;
quote(226) -> <<"%E2">>;
quote(227) -> <<"%E3">>;
quote(228) -> <<"%E4">>;
quote(229) -> <<"%E5">>;
quote(230) -> <<"%E6">>;
quote(231) -> <<"%E7">>;
quote(232) -> <<"%E8">>;
quote(233) -> <<"%E9">>;
quote(234) -> <<"%EA">>;
quote(235) -> <<"%EB">>;
quote(236) -> <<"%EC">>;
quote(237) -> <<"%ED">>;
quote(238) -> <<"%EE">>;
quote(239) -> <<"%EF">>;
quote(240) -> <<"%F0">>;
quote(241) -> <<"%F1">>;
quote(242) -> <<"%F2">>;
quote(243) -> <<"%F3">>;
quote(244) -> <<"%F4">>;
quote(245) -> <<"%F5">>;
quote(246) -> <<"%F6">>;
quote(247) -> <<"%F7">>;
quote(248) -> <<"%F8">>;
quote(249) -> <<"%F9">>;
quote(250) -> <<"%FA">>;
quote(251) -> <<"%FB">>;
quote(252) -> <<"%FC">>;
quote(253) -> <<"%FD">>;
quote(254) -> <<"%FE">>;
quote(255) -> <<"%FF">>.


-spec unquote(binary()) -> char().
unquote(<<"%00">>) -> 0;
unquote(<<"%01">>) -> 1;
unquote(<<"%02">>) -> 2;
unquote(<<"%03">>) -> 3;
unquote(<<"%04">>) -> 4;
unquote(<<"%05">>) -> 5;
unquote(<<"%06">>) -> 6;
unquote(<<"%07">>) -> 7;
unquote(<<"%08">>) -> 8;
unquote(<<"%09">>) -> 9;
unquote(<<"%0A">>) -> 10;
unquote(<<"%0B">>) -> 11;
unquote(<<"%0C">>) -> 12;
unquote(<<"%0D">>) -> 13;
unquote(<<"%0E">>) -> 14;
unquote(<<"%0F">>) -> 15;
unquote(<<"%10">>) -> 16;
unquote(<<"%11">>) -> 17;
unquote(<<"%12">>) -> 18;
unquote(<<"%13">>) -> 19;
unquote(<<"%14">>) -> 20;
unquote(<<"%15">>) -> 21;
unquote(<<"%16">>) -> 22;
unquote(<<"%17">>) -> 23;
unquote(<<"%18">>) -> 24;
unquote(<<"%19">>) -> 25;
unquote(<<"%1A">>) -> 26;
unquote(<<"%1B">>) -> 27;
unquote(<<"%1C">>) -> 28;
unquote(<<"%1D">>) -> 29;
unquote(<<"%1E">>) -> 30;
unquote(<<"%1F">>) -> 31;
unquote(<<"%20">>) -> 32;
unquote(<<"%21">>) -> 33;
unquote(<<"%22">>) -> 34;
unquote(<<"%23">>) -> 35;
unquote(<<"%24">>) -> 36;
unquote(<<"%25">>) -> 37;
unquote(<<"%26">>) -> 38;
unquote(<<"%27">>) -> 39;
unquote(<<"%28">>) -> 40;
unquote(<<"%29">>) -> 41;
unquote(<<"%2A">>) -> 42;
unquote(<<"%2B">>) -> 43;
unquote(<<"%2C">>) -> 44;
unquote(<<"%2D">>) -> 45;
unquote(<<"%2E">>) -> 46;
unquote(<<"%2F">>) -> 47;
unquote(<<"%30">>) -> 48;
unquote(<<"%31">>) -> 49;
unquote(<<"%32">>) -> 50;
unquote(<<"%33">>) -> 51;
unquote(<<"%34">>) -> 52;
unquote(<<"%35">>) -> 53;
unquote(<<"%36">>) -> 54;
unquote(<<"%37">>) -> 55;
unquote(<<"%38">>) -> 56;
unquote(<<"%39">>) -> 57;
unquote(<<"%3A">>) -> 58;
unquote(<<"%3B">>) -> 59;
unquote(<<"%3C">>) -> 60;
unquote(<<"%3D">>) -> 61;
unquote(<<"%3E">>) -> 62;
unquote(<<"%3F">>) -> 63;
unquote(<<"%40">>) -> 64;
unquote(<<"%41">>) -> 65;
unquote(<<"%42">>) -> 66;
unquote(<<"%43">>) -> 67;
unquote(<<"%44">>) -> 68;
unquote(<<"%45">>) -> 69;
unquote(<<"%46">>) -> 70;
unquote(<<"%47">>) -> 71;
unquote(<<"%48">>) -> 72;
unquote(<<"%49">>) -> 73;
unquote(<<"%4A">>) -> 74;
unquote(<<"%4B">>) -> 75;
unquote(<<"%4C">>) -> 76;
unquote(<<"%4D">>) -> 77;
unquote(<<"%4E">>) -> 78;
unquote(<<"%4F">>) -> 79;
unquote(<<"%50">>) -> 80;
unquote(<<"%51">>) -> 81;
unquote(<<"%52">>) -> 82;
unquote(<<"%53">>) -> 83;
unquote(<<"%54">>) -> 84;
unquote(<<"%55">>) -> 85;
unquote(<<"%56">>) -> 86;
unquote(<<"%57">>) -> 87;
unquote(<<"%58">>) -> 88;
unquote(<<"%59">>) -> 89;
unquote(<<"%5A">>) -> 90;
unquote(<<"%5B">>) -> 91;
unquote(<<"%5C">>) -> 92;
unquote(<<"%5D">>) -> 93;
unquote(<<"%5E">>) -> 94;
unquote(<<"%5F">>) -> 95;
unquote(<<"%60">>) -> 96;
unquote(<<"%61">>) -> 97;
unquote(<<"%62">>) -> 98;
unquote(<<"%63">>) -> 99;
unquote(<<"%64">>) -> 100;
unquote(<<"%65">>) -> 101;
unquote(<<"%66">>) -> 102;
unquote(<<"%67">>) -> 103;
unquote(<<"%68">>) -> 104;
unquote(<<"%69">>) -> 105;
unquote(<<"%6A">>) -> 106;
unquote(<<"%6B">>) -> 107;
unquote(<<"%6C">>) -> 108;
unquote(<<"%6D">>) -> 109;
unquote(<<"%6E">>) -> 110;
unquote(<<"%6F">>) -> 111;
unquote(<<"%70">>) -> 112;
unquote(<<"%71">>) -> 113;
unquote(<<"%72">>) -> 114;
unquote(<<"%73">>) -> 115;
unquote(<<"%74">>) -> 116;
unquote(<<"%75">>) -> 117;
unquote(<<"%76">>) -> 118;
unquote(<<"%77">>) -> 119;
unquote(<<"%78">>) -> 120;
unquote(<<"%79">>) -> 121;
unquote(<<"%7A">>) -> 122;
unquote(<<"%7B">>) -> 123;
unquote(<<"%7C">>) -> 124;
unquote(<<"%7D">>) -> 125;
unquote(<<"%7E">>) -> 126;
unquote(<<"%7F">>) -> 127;
unquote(<<"%80">>) -> 128;
unquote(<<"%81">>) -> 129;
unquote(<<"%82">>) -> 130;
unquote(<<"%83">>) -> 131;
unquote(<<"%84">>) -> 132;
unquote(<<"%85">>) -> 133;
unquote(<<"%86">>) -> 134;
unquote(<<"%87">>) -> 135;
unquote(<<"%88">>) -> 136;
unquote(<<"%89">>) -> 137;
unquote(<<"%8A">>) -> 138;
unquote(<<"%8B">>) -> 139;
unquote(<<"%8C">>) -> 140;
unquote(<<"%8D">>) -> 141;
unquote(<<"%8E">>) -> 142;
unquote(<<"%8F">>) -> 143;
unquote(<<"%90">>) -> 144;
unquote(<<"%91">>) -> 145;
unquote(<<"%92">>) -> 146;
unquote(<<"%93">>) -> 147;
unquote(<<"%94">>) -> 148;
unquote(<<"%95">>) -> 149;
unquote(<<"%96">>) -> 150;
unquote(<<"%97">>) -> 151;
unquote(<<"%98">>) -> 152;
unquote(<<"%99">>) -> 153;
unquote(<<"%9A">>) -> 154;
unquote(<<"%9B">>) -> 155;
unquote(<<"%9C">>) -> 156;
unquote(<<"%9D">>) -> 157;
unquote(<<"%9E">>) -> 158;
unquote(<<"%9F">>) -> 159;
unquote(<<"%A0">>) -> 160;
unquote(<<"%A1">>) -> 161;
unquote(<<"%A2">>) -> 162;
unquote(<<"%A3">>) -> 163;
unquote(<<"%A4">>) -> 164;
unquote(<<"%A5">>) -> 165;
unquote(<<"%A6">>) -> 166;
unquote(<<"%A7">>) -> 167;
unquote(<<"%A8">>) -> 168;
unquote(<<"%A9">>) -> 169;
unquote(<<"%AA">>) -> 170;
unquote(<<"%AB">>) -> 171;
unquote(<<"%AC">>) -> 172;
unquote(<<"%AD">>) -> 173;
unquote(<<"%AE">>) -> 174;
unquote(<<"%AF">>) -> 175;
unquote(<<"%B0">>) -> 176;
unquote(<<"%B1">>) -> 177;
unquote(<<"%B2">>) -> 178;
unquote(<<"%B3">>) -> 179;
unquote(<<"%B4">>) -> 180;
unquote(<<"%B5">>) -> 181;
unquote(<<"%B6">>) -> 182;
unquote(<<"%B7">>) -> 183;
unquote(<<"%B8">>) -> 184;
unquote(<<"%B9">>) -> 185;
unquote(<<"%BA">>) -> 186;
unquote(<<"%BB">>) -> 187;
unquote(<<"%BC">>) -> 188;
unquote(<<"%BD">>) -> 189;
unquote(<<"%BE">>) -> 190;
unquote(<<"%BF">>) -> 191;
unquote(<<"%C0">>) -> 192;
unquote(<<"%C1">>) -> 193;
unquote(<<"%C2">>) -> 194;
unquote(<<"%C3">>) -> 195;
unquote(<<"%C4">>) -> 196;
unquote(<<"%C5">>) -> 197;
unquote(<<"%C6">>) -> 198;
unquote(<<"%C7">>) -> 199;
unquote(<<"%C8">>) -> 200;
unquote(<<"%C9">>) -> 201;
unquote(<<"%CA">>) -> 202;
unquote(<<"%CB">>) -> 203;
unquote(<<"%CC">>) -> 204;
unquote(<<"%CD">>) -> 205;
unquote(<<"%CE">>) -> 206;
unquote(<<"%CF">>) -> 207;
unquote(<<"%D0">>) -> 208;
unquote(<<"%D1">>) -> 209;
unquote(<<"%D2">>) -> 210;
unquote(<<"%D3">>) -> 211;
unquote(<<"%D4">>) -> 212;
unquote(<<"%D5">>) -> 213;
unquote(<<"%D6">>) -> 214;
unquote(<<"%D7">>) -> 215;
unquote(<<"%D8">>) -> 216;
unquote(<<"%D9">>) -> 217;
unquote(<<"%DA">>) -> 218;
unquote(<<"%DB">>) -> 219;
unquote(<<"%DC">>) -> 220;
unquote(<<"%DD">>) -> 221;
unquote(<<"%DE">>) -> 222;
unquote(<<"%DF">>) -> 223;
unquote(<<"%E0">>) -> 224;
unquote(<<"%E1">>) -> 225;
unquote(<<"%E2">>) -> 226;
unquote(<<"%E3">>) -> 227;
unquote(<<"%E4">>) -> 228;
unquote(<<"%E5">>) -> 229;
unquote(<<"%E6">>) -> 230;
unquote(<<"%E7">>) -> 231;
unquote(<<"%E8">>) -> 232;
unquote(<<"%E9">>) -> 233;
unquote(<<"%EA">>) -> 234;
unquote(<<"%EB">>) -> 235;
unquote(<<"%EC">>) -> 236;
unquote(<<"%ED">>) -> 237;
unquote(<<"%EE">>) -> 238;
unquote(<<"%EF">>) -> 239;
unquote(<<"%F0">>) -> 240;
unquote(<<"%F1">>) -> 241;
unquote(<<"%F2">>) -> 242;
unquote(<<"%F3">>) -> 243;
unquote(<<"%F4">>) -> 244;
unquote(<<"%F5">>) -> 245;
unquote(<<"%F6">>) -> 246;
unquote(<<"%F7">>) -> 247;
unquote(<<"%F8">>) -> 248;
unquote(<<"%F9">>) -> 249;
unquote(<<"%FA">>) -> 250;
unquote(<<"%FB">>) -> 251;
unquote(<<"%FC">>) -> 252;
unquote(<<"%FD">>) -> 253;
unquote(<<"%FE">>) -> 254;
unquote(<<"%FF">>) -> 255.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


split_test_() ->
    [
        {"empty binary", fun() ->
            ?assertEqual([<<>>], split(<<>>, <<"w">>))
        end},
        {"splitting whole binary", fun() ->
            ?assertEqual([<<"a">>, <<>>, <<"b">>, <<>>, <<"c">>], split(<<"a::::b::::c">>, <<"::">>))
        end},
        {"splitting only once", fun() ->
            ?assertEqual([<<"a">>, <<"::b::::c">>], split(<<"a::::b::::c">>, <<"::">>, 1))
        end},
        {"binary consists only of separator", fun() ->
            ?assertEqual([<<>>, <<>>, <<>>], split(<<"::::">>, <<"::">>))
        end},
        {"no splits", fun() ->
            ?assertEqual([<<";;:;">>], split(<<";;:;">>, <<"::">>))
        end}
    ].

-endif.
