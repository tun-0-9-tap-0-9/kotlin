// !LANGUAGE: +CompileTimeCalculations

const val byteMax = { java.lang.Byte.MAX_VALUE }()
const val byteMin = { java.lang.Byte.MIN_VALUE }()
const val byteSize = { java.lang.Byte.SIZE }()
const val byteBytes = { java.lang.Byte.BYTES }()

const val intMax = { java.lang.Integer.MAX_VALUE }()
const val intMin = { java.lang.Integer.MIN_VALUE }()
const val intSize = { java.lang.Integer.SIZE }()
const val intBytes = { java.lang.Integer.BYTES }()

const val floatMax = { java.lang.Float.MAX_VALUE }()
const val floatMin = { java.lang.Float.MIN_VALUE }()
const val floatSize = { java.lang.Float.SIZE }()
const val floatBytes = { java.lang.Float.BYTES }()
const val floatMaxExponent = { java.lang.Float.MAX_EXPONENT }()
const val floatMinExponent = { java.lang.Float.MIN_EXPONENT }()
const val floatNegInfinity = { java.lang.Float.NEGATIVE_INFINITY }()
const val floatPosInfinity = { java.lang.Float.POSITIVE_INFINITY }()

const val booleanFalse = { java.lang.Boolean.FALSE }().toString()
const val booleanTrue = { java.lang.Boolean.TRUE }().toString()

const val charMax = { java.lang.Character.MAX_VALUE }().toInt()
const val charMin = { java.lang.Character.MIN_VALUE }().toInt()
const val charSize = { java.lang.Character.SIZE }()
const val charBytes = { java.lang.Character.BYTES }()
