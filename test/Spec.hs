import            Data.Bits
import            Data.Bits.ByteString
import qualified  Data.ByteString as B
import            Test.Hspec
import            Test.QuickCheck hiding ((.&.))

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack (listOf arbitrary)

prop_IdentityAND :: B.ByteString -> Bool
prop_IdentityAND a = a == (a .&. a)

prop_CommutativityAND :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityAND a b = (a .&. b) == (b .&. a)

prop_AssociativityAND :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityAND a b c = (a .&. (b .&. c)) == ((a .&. b) .&. c)

prop_IdentityOR :: B.ByteString -> Bool
prop_IdentityOR a = a == (a .|. (B.replicate (B.length a) 0))

prop_CommutativityOR :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityOR a b = (a .|. b) == (b .|. a)

prop_AssociativityOR :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityOR a b c = (a .|. (b .|. c)) == ((a .|. b) .|. c)

prop_IdentityXOR :: B.ByteString -> Bool
prop_IdentityXOR a = a == (a `xor` (B.replicate (B.length a) 0))

prop_CommutativityXOR :: B.ByteString -> B.ByteString -> Bool
prop_CommutativityXOR a b = (a `xor` b) == (b `xor` a)

prop_AssociativityXOR :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
prop_AssociativityXOR a b c = (a `xor` (b `xor` c)) == ((a `xor` b) `xor` c)

prop_IdentityComplement :: B.ByteString -> Bool
prop_IdentityComplement x = x == complement (complement x)

prop_ConstantLengthShiftL :: B.ByteString -> Int -> Bool
prop_ConstantLengthShiftL x i = (B.length x) == (B.length $ shiftL x i)

prop_ConstantLengthShiftR :: B.ByteString -> Int -> Bool
prop_ConstantLengthShiftR x i = (B.length x) == (B.length $ shiftR x i)

prop_ConstantLengthShift :: B.ByteString -> Int -> Bool
prop_ConstantLengthShift x i = (B.length x) == (B.length $ shift x i)

prop_ConstantLengthRotateL :: B.ByteString -> Int -> Bool
prop_ConstantLengthRotateL x i = (B.length x) == (B.length $ rotateL x i)

prop_ConstantLengthRotateR :: B.ByteString -> Int -> Bool
prop_ConstantLengthRotateR x i = (B.length x) == (B.length $ rotateR x i)

prop_ConstantLengthRotate :: B.ByteString -> Int -> Bool
prop_ConstantLengthRotate x i = (B.length x) == (B.length $ rotate x i)

prop_IdentityRotate :: B.ByteString -> Int -> Bool
prop_IdentityRotate x i = x == rotateR (rotateL x i) i

prop_ConstantLengthSetBit :: B.ByteString -> Int -> Bool
prop_ConstantLengthSetBit x i = (B.length x) == (B.length $ setBit x i)

main :: IO ()
main = hspec $ do
  describe "AND" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityAND
      it "Commutativity" $ property prop_CommutativityAND
      it "Associativity" $ property prop_AssociativityAND
  describe "OR" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityOR
      it "Commutativity" $ property prop_CommutativityOR
      it "Associativity" $ property prop_AssociativityOR
  describe "XOR" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityXOR
      it "Commutativity" $ property prop_CommutativityXOR
      it "Associativity" $ property prop_AssociativityXOR
  describe "Complement" $ do
    context "Should have the following properties:" $ do
      it "Identity" $ property prop_IdentityComplement
  describe "Shift" $ do
    context "Should have the following properties:" $ do
      it "Length should not change on ShiftL" $ property prop_ConstantLengthShiftL
      it "Length should not change on ShiftR" $ property prop_ConstantLengthShiftR
      it "Length should not change on Shift" $ property prop_ConstantLengthShift
  describe "Rotate" $ do
    context "Should have the following properties:" $ do
      it "Length should not change on RotateL" $ property prop_ConstantLengthRotateL
      it "Length should not change on RotateR" $ property prop_ConstantLengthRotateR
      it "Length should not change on Rotate" $ property prop_ConstantLengthRotate
      it "Identity" $ property prop_IdentityRotate
  describe "SetBit" $ do
    context "Should have the following properties:" $ do
      it "Length should not change on SetBit" $ property prop_ConstantLengthSetBit
