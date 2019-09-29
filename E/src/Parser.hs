{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import Grammar
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,163) ([0,49574,0,1688,3,6752,12,1024,64,0,0,0,0,0,0,0,0,0,0,0,24576,0,16384,0,4096,0,42496,193,38912,774,0,0,0,4096,0,16384,0,16,0,64,0,0,0,0,0,36864,256,16384,1026,0,128,0,512,0,32,0,32896,1,0,0,0,0,36864,256,16384,1026,0,4105,0,49574,0,1688,3,6752,12,27008,48,42496,193,0,0,0,0,0,0,0,0,0,0,0,32768,0,2304,16,0,4,0,1,0,0,0,0,0,49574,0,1688,3,32768,0,1536,0,6144,0,0,0,16384,1026,0,0,0,0,0,0,0,32896,0,0,2,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProblem","%start_parseExpression","Problem","Context","Expression","Disjunction","Conjunction","Unary","Predicate","Term","Slag","Multi","Terms","or","and","not","open","close","comma","zero","inc","forall","exist","point","mul","plus","eq","impl","var","pred","turn","%eof"]
        bit_start = st * 34
        bit_end = (st + 1) * 34
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..33]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (18) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (24) = happyShift action_15
action_0 (25) = happyShift action_16
action_0 (31) = happyShift action_17
action_0 (32) = happyShift action_18
action_0 (5) = happyGoto action_20
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 (13) = happyGoto action_10
action_0 (14) = happyGoto action_11
action_0 _ = happyReduce_5

action_1 (18) = happyShift action_12
action_1 (19) = happyShift action_13
action_1 (22) = happyShift action_14
action_1 (24) = happyShift action_15
action_1 (25) = happyShift action_16
action_1 (31) = happyShift action_17
action_1 (32) = happyShift action_18
action_1 (7) = happyGoto action_19
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 (12) = happyGoto action_9
action_1 (13) = happyGoto action_10
action_1 (14) = happyGoto action_11
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (18) = happyShift action_12
action_2 (19) = happyShift action_13
action_2 (22) = happyShift action_14
action_2 (24) = happyShift action_15
action_2 (25) = happyShift action_16
action_2 (31) = happyShift action_17
action_2 (32) = happyShift action_18
action_2 (6) = happyGoto action_3
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (9) = happyGoto action_6
action_2 (10) = happyGoto action_7
action_2 (11) = happyGoto action_8
action_2 (12) = happyGoto action_9
action_2 (13) = happyGoto action_10
action_2 (14) = happyGoto action_11
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (21) = happyShift action_35
action_3 (33) = happyShift action_36
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (16) = happyShift action_33
action_5 (30) = happyShift action_34
action_5 _ = happyReduce_6

action_6 (17) = happyShift action_32
action_6 _ = happyReduce_8

action_7 _ = happyReduce_10

action_8 _ = happyReduce_12

action_9 (28) = happyShift action_30
action_9 (29) = happyShift action_31
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (27) = happyShift action_29
action_10 _ = happyReduce_20

action_11 (23) = happyShift action_28
action_11 _ = happyReduce_22

action_12 (18) = happyShift action_12
action_12 (19) = happyShift action_13
action_12 (22) = happyShift action_14
action_12 (24) = happyShift action_15
action_12 (25) = happyShift action_16
action_12 (31) = happyShift action_17
action_12 (32) = happyShift action_18
action_12 (10) = happyGoto action_27
action_12 (11) = happyGoto action_8
action_12 (12) = happyGoto action_9
action_12 (13) = happyGoto action_10
action_12 (14) = happyGoto action_11
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (18) = happyShift action_12
action_13 (19) = happyShift action_13
action_13 (22) = happyShift action_14
action_13 (24) = happyShift action_15
action_13 (25) = happyShift action_16
action_13 (31) = happyShift action_17
action_13 (32) = happyShift action_18
action_13 (7) = happyGoto action_25
action_13 (8) = happyGoto action_5
action_13 (9) = happyGoto action_6
action_13 (10) = happyGoto action_7
action_13 (11) = happyGoto action_8
action_13 (12) = happyGoto action_26
action_13 (13) = happyGoto action_10
action_13 (14) = happyGoto action_11
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_27

action_15 (31) = happyShift action_24
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (31) = happyShift action_23
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (19) = happyShift action_22
action_17 _ = happyReduce_24

action_18 (19) = happyShift action_21
action_18 _ = happyReduce_17

action_19 (34) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (34) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (19) = happyShift action_43
action_21 (22) = happyShift action_14
action_21 (31) = happyShift action_17
action_21 (12) = happyGoto action_50
action_21 (13) = happyGoto action_10
action_21 (14) = happyGoto action_11
action_21 (15) = happyGoto action_52
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (19) = happyShift action_43
action_22 (22) = happyShift action_14
action_22 (31) = happyShift action_17
action_22 (12) = happyGoto action_50
action_22 (13) = happyGoto action_10
action_22 (14) = happyGoto action_11
action_22 (15) = happyGoto action_51
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (26) = happyShift action_49
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (26) = happyShift action_48
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (20) = happyShift action_47
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (20) = happyShift action_46
action_26 (28) = happyShift action_30
action_26 (29) = happyShift action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_13

action_28 _ = happyReduce_28

action_29 (19) = happyShift action_43
action_29 (22) = happyShift action_14
action_29 (31) = happyShift action_17
action_29 (14) = happyGoto action_45
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (19) = happyShift action_43
action_30 (22) = happyShift action_14
action_30 (31) = happyShift action_17
action_30 (13) = happyGoto action_44
action_30 (14) = happyGoto action_11
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (19) = happyShift action_43
action_31 (22) = happyShift action_14
action_31 (31) = happyShift action_17
action_31 (12) = happyGoto action_42
action_31 (13) = happyGoto action_10
action_31 (14) = happyGoto action_11
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (18) = happyShift action_12
action_32 (19) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (24) = happyShift action_15
action_32 (25) = happyShift action_16
action_32 (31) = happyShift action_17
action_32 (32) = happyShift action_18
action_32 (10) = happyGoto action_41
action_32 (11) = happyGoto action_8
action_32 (12) = happyGoto action_9
action_32 (13) = happyGoto action_10
action_32 (14) = happyGoto action_11
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (18) = happyShift action_12
action_33 (19) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (24) = happyShift action_15
action_33 (25) = happyShift action_16
action_33 (31) = happyShift action_17
action_33 (32) = happyShift action_18
action_33 (9) = happyGoto action_40
action_33 (10) = happyGoto action_7
action_33 (11) = happyGoto action_8
action_33 (12) = happyGoto action_9
action_33 (13) = happyGoto action_10
action_33 (14) = happyGoto action_11
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (18) = happyShift action_12
action_34 (19) = happyShift action_13
action_34 (22) = happyShift action_14
action_34 (24) = happyShift action_15
action_34 (25) = happyShift action_16
action_34 (31) = happyShift action_17
action_34 (32) = happyShift action_18
action_34 (7) = happyGoto action_39
action_34 (8) = happyGoto action_5
action_34 (9) = happyGoto action_6
action_34 (10) = happyGoto action_7
action_34 (11) = happyGoto action_8
action_34 (12) = happyGoto action_9
action_34 (13) = happyGoto action_10
action_34 (14) = happyGoto action_11
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (18) = happyShift action_12
action_35 (19) = happyShift action_13
action_35 (22) = happyShift action_14
action_35 (24) = happyShift action_15
action_35 (25) = happyShift action_16
action_35 (31) = happyShift action_17
action_35 (32) = happyShift action_18
action_35 (7) = happyGoto action_38
action_35 (8) = happyGoto action_5
action_35 (9) = happyGoto action_6
action_35 (10) = happyGoto action_7
action_35 (11) = happyGoto action_8
action_35 (12) = happyGoto action_9
action_35 (13) = happyGoto action_10
action_35 (14) = happyGoto action_11
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (18) = happyShift action_12
action_36 (19) = happyShift action_13
action_36 (22) = happyShift action_14
action_36 (24) = happyShift action_15
action_36 (25) = happyShift action_16
action_36 (31) = happyShift action_17
action_36 (32) = happyShift action_18
action_36 (7) = happyGoto action_37
action_36 (8) = happyGoto action_5
action_36 (9) = happyGoto action_6
action_36 (10) = happyGoto action_7
action_36 (11) = happyGoto action_8
action_36 (12) = happyGoto action_9
action_36 (13) = happyGoto action_10
action_36 (14) = happyGoto action_11
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_2

action_38 _ = happyReduce_4

action_39 _ = happyReduce_7

action_40 (17) = happyShift action_32
action_40 _ = happyReduce_9

action_41 _ = happyReduce_11

action_42 (28) = happyShift action_30
action_42 _ = happyReduce_19

action_43 (19) = happyShift action_43
action_43 (22) = happyShift action_14
action_43 (31) = happyShift action_17
action_43 (12) = happyGoto action_58
action_43 (13) = happyGoto action_10
action_43 (14) = happyGoto action_11
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (27) = happyShift action_29
action_44 _ = happyReduce_21

action_45 (23) = happyShift action_28
action_45 _ = happyReduce_23

action_46 _ = happyReduce_26

action_47 _ = happyReduce_14

action_48 (18) = happyShift action_12
action_48 (19) = happyShift action_13
action_48 (22) = happyShift action_14
action_48 (24) = happyShift action_15
action_48 (25) = happyShift action_16
action_48 (31) = happyShift action_17
action_48 (32) = happyShift action_18
action_48 (7) = happyGoto action_57
action_48 (8) = happyGoto action_5
action_48 (9) = happyGoto action_6
action_48 (10) = happyGoto action_7
action_48 (11) = happyGoto action_8
action_48 (12) = happyGoto action_9
action_48 (13) = happyGoto action_10
action_48 (14) = happyGoto action_11
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (18) = happyShift action_12
action_49 (19) = happyShift action_13
action_49 (22) = happyShift action_14
action_49 (24) = happyShift action_15
action_49 (25) = happyShift action_16
action_49 (31) = happyShift action_17
action_49 (32) = happyShift action_18
action_49 (7) = happyGoto action_56
action_49 (8) = happyGoto action_5
action_49 (9) = happyGoto action_6
action_49 (10) = happyGoto action_7
action_49 (11) = happyGoto action_8
action_49 (12) = happyGoto action_9
action_49 (13) = happyGoto action_10
action_49 (14) = happyGoto action_11
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (28) = happyShift action_30
action_50 _ = happyReduce_29

action_51 (20) = happyShift action_55
action_51 (21) = happyShift action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (20) = happyShift action_53
action_52 (21) = happyShift action_54
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_18

action_54 (19) = happyShift action_43
action_54 (22) = happyShift action_14
action_54 (31) = happyShift action_17
action_54 (12) = happyGoto action_59
action_54 (13) = happyGoto action_10
action_54 (14) = happyGoto action_11
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_25

action_56 _ = happyReduce_16

action_57 _ = happyReduce_15

action_58 (20) = happyShift action_46
action_58 (28) = happyShift action_30
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (28) = happyShift action_30
action_59 _ = happyReduce_30

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Problem happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : []
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_3 : happy_var_1
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Impl happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Or happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Not happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 10 happyReduction_15
happyReduction_15 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Exist happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (TPred happy_var_1))
	 =  HappyAbsSyn11
		 (PVar happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPred happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Pred happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn14
		 (Var happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 14 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Func happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn14
		 (Zero
	)

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (Inc happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : []
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSym '|' -> cont 16;
	TSym '&' -> cont 17;
	TSym '!' -> cont 18;
	TSym '(' -> cont 19;
	TSym ')' -> cont 20;
	TSym ',' -> cont 21;
	TSym '0' -> cont 22;
	TSym '\'' -> cont 23;
	TSym '@' -> cont 24;
	TSym '?' -> cont 25;
	TSym '.' -> cont 26;
	TSym '*' -> cont 27;
	TSym '+' -> cont 28;
	TSym '=' -> cont 29;
	TImpl -> cont 30;
	TVar happy_dollar_dollar -> cont 31;
	TPred happy_dollar_dollar -> cont 32;
	TTurn -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseProblem tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseExpression tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDoSeq


parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
