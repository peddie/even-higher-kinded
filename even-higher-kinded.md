---
title: "Real-world applications for even higher-kinded data types"
author: Matt Peddie, Kittyhawk <mpeddie@gmail.com>
date: 15 January 2019
output:
    slidy_presentation:
      highlight: haddock
---

# Apologies in advance

This is not a beginner-level talk, and I don't have time in 15 minutes to go
over the basics.

We're going to talk about some types with complicated kinds.  The messages I
want to convey are:

 - Types with complex kinds can arise naturally in real-world situations.

 - Factoring out the components of large, nested types can illuminate useful
   structural patterns in spite of the fact that the kinds get hairier.

 - The existing standard library still provides a good guide for figuring out
   the right abstractions.

# Kinds

    Int    :: *
    Maybe  :: * -> *
    Either :: * -> * -> *
    ???    :: (* -> *) -> *

# Higher-kinded structures (from last time)

    data X f = MakeMeAnX (f Word)

    f :: (* -> *)
    X :: (* -> *) -> *

# Application (from last time)

C code generation:

    type CGPrimitives = '[Bool, Int8, . . ., Double]

<!-- -->

    data PrimitiveCollection f = PrimitiveCollection
        { primCollBool :: f Bool
        , primCollInt16 :: f Int16
        . . .
        , primCollDouble :: f Double
        }

where `f :: * -> *` is a type that allows us to either evaluate or
code-generate.  It gives us a typed DSL.

# Abstractions (from last time)

    class HK1Functor x where
        type HK1Constraint x a :: Constraint

        fmapHK1 :: (forall a. HK1Constraint x a => f a -> g a)
                -> x f
                -> x g

    class HK1Functor x => HK1Traverse x where
        traverseHK1 :: Applicative m
                    => (forall a. HK1Constraint x a => f a -> m (g a))
                    -> x f
                    -> m (x g)

    newtype HK1Function x f g a =
      HK1Function { runHK1Function :: HK1Constraint x a => f a -> g a }

    class HK1Functor x => HK1Apply x where
      apHK1 :: x (HK1Function x f g) -> x f -> x g

Akin perhaps to either a `Naperian` functor or a `Representable` functor, if the
`HK1Constraint` is sufficiently uninteresting.

# New application: heterogeneous sensor processing

Uniform processing of heterogeneous collections: sensor inputs to flight
controller.

    data PitotTube f = PitotTube
      { staticPort :: f Double
      , stagnationPort :: f Double
      }

    data Magnetometer f = Magnetometer
      { bfieldX :: f Double
      , bfieldY :: f Double
      , bfieldZ :: f Double
      }

# New application: heterogeneous sensor processing

### Many processing steps:

 - ensure that we got a new measurement (or report an error)
 - filter out outliers
 - flag any unexpected conditions
 - update estimate
 - collect statistics

# New application: heterogeneous sensor processing

### Many processing steps:

 - ensure that we got a new measurement (or report an error)
 - filter out outliers
 - flag any unexpected conditions
 - update estimate
 - collect statistics

### Goals when adding or removing a sensor:

  - should only have to touch code related to that sensor
  - compiler should constrain us to the correct processing path (we can't forget
    any steps)

# Excelsior

    {-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies, RankNTypes #-}

Write one type specifying all the sensors on the aircraft.

    data Sensors (s:: ((* -> *) -> *) -> (* -> *) -> *) (f :: * -> *) = Sensors
      { sensorsPitot        :: s PitotTube f
      , sensorsMagnetometer :: s Magnetometer f
      }

# Excelsior

    {-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies, RankNTypes #-}

Write one type specifying all the sensors on the aircraft.

    data Sensors (s:: ((* -> *) -> *) -> (* -> *) -> *) (f :: * -> *) = Sensors
      { sensorsPitot        :: s PitotTube f
      , sensorsMagnetometer :: s Magnetometer f
      }

Example type for s:

    data Timestamp (x :: (* -> *) -> *) (f :: * -> *) = Timestamp
      { timestampMeasurement :: x f
      , timestampEpoch       :: f Double
      }

# Excelsior

    {-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies, RankNTypes #-}

Write one type specifying all the sensors on the aircraft.

    data Sensors (s:: ((* -> *) -> *) -> (* -> *) -> *) (f :: * -> *) = Sensors
      { sensorsPitot        :: s PitotTube f
      , sensorsMagnetometer :: s Magnetometer f
      }

Example type for s:

    data Timestamp (x :: (* -> *) -> *) (f :: * -> *) = Timestamp
      { timestampMeasurement :: x f
      , timestampEpoch       :: f Double
      }

The kinds are even higher:

    x       ::                                       (* -> *) -> *  -- HK1
    s       :: ((* -> *) -> *)  ->                   (* -> *) -> *  -- HK2
    Sensors :: (((* -> *) -> *) -> (* -> *) -> *) -> (* -> *) -> *  -- HK3

# Excelsior

    {-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies, RankNTypes #-}

Write one type specifying all the sensors on the aircraft.

    data Sensors (s:: ((* -> *) -> *) -> (* -> *) -> *) (f :: * -> *) = Sensors
      { sensorsPitot        :: s PitotTube f
      , sensorsMagnetometer :: s Magnetometer f
      }

Example type for s:

    data Timestamp (x :: (* -> *) -> *) (f :: * -> *) = Timestamp
      { timestampMeasurement :: x f
      , timestampEpoch       :: f Double
      }

The kinds are even higher:

    x       ::                                       (* -> *) -> *  -- HK1
    s       :: ((* -> *) -> *)  ->                   (* -> *) -> *  -- HK2
    Sensors :: (((* -> *) -> *) -> (* -> *) -> *) -> (* -> *) -> *  -- HK3

Conjecture: if we can write all the sensor processing steps as operations on
`Sensors`, and find a way to connect all the corresponding pieces (individual
steps for individual sensors) together in a way that doesn't let us leave
anything out, we can meet the requirements.

# Application: latest measurement time

Get latest measurement time for all sensors.

    newtype ConstHK2 a (x :: (* -> *) -> *) (f :: * -> *) = ConstHK2 { getConstHK2 :: a }

    getLatestMeasurementTime :: Sensors Timestamp f -> Sensors (ConstHK2 (f Double)) f

# Application: latest measurement time

Get latest measurement time for all sensors.

    newtype ConstHK2 a (x :: (* -> *) -> *) (f :: * -> *) = ConstHK2 { getConstHK2 :: a }

    getLatestMeasurementTime :: Sensors Timestamp f -> Sensors (ConstHK2 (f Double)) f

This looks suspiciously familiar:

    class HK3Functor (v :: (((* -> *) -> *) -> (* -> *) -> *) -> (* -> *) -> *) where
      type HK3Constraint v (f :: * -> *) (x :: (* -> *) -> *) :: Constraint

      fmapHK3 :: (forall x. HK3Constraint v f x => g x f -> h x f) -> v g f -> v h f

    instance HK3Functor Sensors where
      type HK3Constraint Sensors f x = <various constraints>

      fmapHK3 f (Sensors oldPitot oldMagnetometer) = Sensors
          { sensorsPitot        = f oldPitot
          , sensorsMagnetometer = f oldMagnetometer
          }

# Application: latest measurement time

Get latest measurement time for all sensors.

    class HK3Functor (v :: (((* -> *) -> *) -> (* -> *) -> *) -> (* -> *) -> *) where
      type HK3Constraint v (f :: * -> *) (x :: (* -> *) -> *) :: Constraint

      fmapHK3 :: (forall x. HK3Constraint v f x => g x f -> h x f) -> v g f -> v h f

    instance HK3Functor Sensors where
      type HK3Constraint Sensors f x = <various constraints>

      fmapHK3 f (Sensors oldPitot oldMagnetometer) = Sensors
          { sensorsPitot        = f oldPitot
          , sensorsMagnetometer = f oldMagnetometer
          }

Now:

    getLatestMeasurementTime :: Sensors Timestamp f -> Sensors (ConstHK2 (f Double)) f
    getLatestMeasurementTime = fmapHK3 (ConstHK2 . timestampEpoch)

# Application: latest measurement time

Check against the requirements:

 - only write the code necessary for each sensor
   - add field to `instance HK3Functor`
 - compiler should prevent us from forgetting any processing steps
   - type error if timestamp isn't passed in!

# Application: checksum validation

Collect checksum errors from all sensor measurements.

    newtype ComposeHK2
      (s :: ((* -> *) -> *) -> (* -> *) -> *)
      (t :: ((* -> *) -> *) -> (* -> *) -> *)
      (x :: (* -> *) -> *)
      (f :: * -> *)
      = ComposeHK2 { getComposeHK2 :: s (t x) f }

    data Checksum (x :: (* -> *) -> *) (f :: * -> *) = Checksum
      { checksumCRC32 :: f Word32
      , checksumData  :: x f
      }

    checksumVerify :: Sensors (ComposeHK2 Checksum Timestamp) f
                   -> Validation [SensorError] (Sensors Timestamp f)

# Application: checksum validation

    checksumVerify :: Sensors (ComposeHK2 Checksum Timestamp) f
                   -> Validation [SensorError] (Sensors Timestamp f)

This looks awfully familiar as well.

    class HK3Functor v => HK3Traversable v where
      traverseHK3 :: Applicative m
                  => (forall x. HK3Constraint v f x => g x f -> m (h x f))
                  -> v g f
                  -> m (v h f)

    instance HK3Traversable Sensors where
      traverseHK3 f (Sensors oldPitot oldMagnetometer) =
        Sensors <$> f oldPitot <*> f oldMagnetometer

# Application: checksum validation

    checksumVerify :: Sensors (ComposeHK2 Checksum Timestamp) f
                   -> Validation [SensorError] (Sensors Timestamp f)

This looks awfully familiar as well.

    class HK3Functor v => HK3Traversable v where
      traverseHK3 :: Applicative m
                  => (forall x. HK3Constraint v f x => g x f -> m (h x f))
                  -> v g f
                  -> m (v h f)

    instance HK3Traversable Sensors where
      traverseHK3 f (Sensors oldPitot oldMagnetometer) =
        Sensors <$> f oldPitot <*> f oldMagnetometer

In this case, we must use a helper type class (`CRC32able`) to operate uniformly
on different measurements.

      type HK3Constraint Sensors f x = CRC32able (x f)

# Application: checksum validation

    checksumVerify :: Sensors (ComposeHK2 Checksum Timestamp) f
                   -> Validation [SensorError] (Sensors Timestamp f)
    checksumVerify = traverseHK3 go
      where
        go (ComposeHK2 cksummed) =
          validate someError
          (\tsMeas -> checksumCRC32 cksummed .== crc32 tsMeas)
          (checksumData cksummed)

# Application: checksum validation

Check against the requirements:

 - only write the code necessary for each sensor
   - `instance CRC32able a`
   - add field to `instance HK3Traversable Sensors`
 - compiler should prevent us from forgetting any processing steps
   - compile error if no `CRC32able` instance is provided!

# Application: stateful filtering

Stateful filtering of sensor measurements.

    class SensorMeasurement x where
      type FilterState  x (f :: * -> *) :: *
      type FilterConfig x (f :: * -> *) :: *
      type Estimate     x (f :: * -> *) :: *

      filterStep :: FilterConfig x f                        -- ^ Config settings
                 -> FilterState x f                         -- ^ previous state
                 -> x f                                     -- ^ latest measurement
                 -> (FilterState x f, Estimate x f)         -- ^ Resulting estimate and next state

NB: cannot partially apply type synonyms.

# Application: stateful filtering

Stateful filtering of sensor measurements.

    class SensorMeasurement x where
      type FilterState  x (f :: * -> *) :: *
      type FilterConfig x (f :: * -> *) :: *
      type Estimate     x (f :: * -> *) :: *

      filterStep :: FilterConfig x f                        -- ^ Config settings
                 -> FilterState x f                         -- ^ previous state
                 -> x f                                     -- ^ latest measurement
                 -> (FilterState x f, Estimate x f)         -- ^ Resulting estimate and next state

NB: cannot partially apply type synonyms.

    newtype FilterStateF x f  = FilterStateF { getFilterStateF :: FilterState x f }
    newtype FilterConfigF x f = FilterConfigF { getFilterConfigF :: FilterConfig x f }
    newtype EstimateF x f     = EstimateF { getEstimateF :: Estimate x f }

<!--  -->

      type HK3Constraint Sensors f x = (CRC32able (x f), SensorMeasurement x)

    filterSensors :: Sensors FilterConfigF f
                  -> Sensors FilterStateF f
                  -> Sensors Timestamp f
                  -> Sensors (Product2 FilterStateF EstimateF) f

# Application: stateful filtering

Let's let the standard library guide the way again.  A multi-argument function
applied in context?

    newtype HK3Function
      (v :: (((* -> *) -> *) -> (* -> *) -> *) -> (* -> *) -> *)
      (g :: ((* -> *) -> *) -> (* -> *) -> *)
      (h :: ((* -> *) -> *) -> (* -> *) -> *)
      (x :: (* -> *) -> *)
      (f :: * -> *) = HK3Function
      { runHK3Function :: HK3Constraint v f x => g x f -> h x f }

    class HK3Functor v => HK3Apply v where
      apHK3 :: v (HK3Function v g h) f -> v g f -> v h f

    instance HK3Apply Sensors where
      apHK3 f x = Sensors
        { sensorsPitot        = runHK3Function (sensorsPitot f) $ sensorsPitot x
        , sensorsMagnetometer = runHK3Function (sensorsMagnetometer f) $ sensorsMagnetometer x
        }

# Application: stateful filtering

    {-# LANGUAGE ScopedTypeVariables #-}

    zipWith3HK3 :: forall v g h j k f. HK3Apply v
              => (forall x. HK3Constraint v f x => g x f -> h x f -> j x f -> k x f)
              -> v g f -> v h f -> v j f -> v k f
    zipWith3HK3 f vgf vhf vjf =
      fmapHK3 wrap vgf `apHK3` vhf `apHK3` vjf
      where
        wrap :: g x f -> HK3Function v h (HK3Function v j k) x f
        wrap gxfs = HK3Function $ \hxfs -> HK3Function $ f gxfs hxfs

# Application: stateful filtering

    {-# LANGUAGE ScopedTypeVariables #-}

    zipWith3HK3 :: forall v g h j k f. HK3Apply v
              => (forall x. HK3Constraint v f x => g x f -> h x f -> j x f -> k x f)
              -> v g f -> v h f -> v j f -> v k f
    zipWith3HK3 f vgf vhf vjf =
      fmapHK3 wrap vgf `apHK3` vhf `apHK3` vjf
      where
        wrap :: g x f -> HK3Function v h (HK3Function v j k) x f
        wrap gxfs = HK3Function $ \hxfs -> HK3Function $ f gxfs hxfs

<!--  -->

    data Product2
      (g :: ((* -> *) -> *) -> (* -> *) -> *)
      (h :: ((* -> *) -> *) -> (* -> *) -> *)
      (x :: (* -> *) -> *)
      (f :: * -> *) = Pair2
      { pair2Left :: g x f
      , pair2Right :: h x f
      }

# Application: stateful filtering

    filterSensors :: Sensors FilterConfigF f
                  -> Sensors FilterStateF f
                  -> Sensors Timestamp f
                  -> Sensors (Product2 FilterStateF EstimateF) f
    filterSensors = zipWith3HK3 go
      where
        go (FilterConfigF config) (FilterStateF oldState) (Timestamp meas _) =
          let (newState, estimate) = filterStep config oldState meas in
            Pair2 (FilterStateF newState) (EstimateF estimate)

# Application: stateful filtering

    filterSensors :: Sensors FilterConfigF f
                  -> Sensors FilterStateF f
                  -> Sensors Timestamp f
                  -> Sensors (Product2 FilterStateF EstimateF) f
    filterSensors = zipWith3HK3 go
      where
        go (FilterConfigF config) (FilterStateF oldState) (Timestamp meas _) =
          let (newState, estimate) = filterStep config oldState meas in
            Pair2 (FilterStateF newState) (EstimateF estimate)

<!--  -->

    getStates = fmapHK3 pair2Left

    getEstimates = fmapHK3 pair2Right

# Application: stateful filtering

Check against the requirements:

 - only write the code necessary for each sensor
   - `instance SensorMeasurement a` (including associated type synonyms)
   - no defaults in type class
   - add field to `instance HK3Apply Sensors`
 - compiler should prevent us from forgetting any processing steps
   - compile error if no `SensorMeasurement` instance is provided!

# Limitations

Many standard classes will not deal well with this kind of code.

You may need to create your own "lifted" versions of standard type classes (or
upgrade to GHC 8.6 and play with `-XQuantifiedConstraints`).

This may look like the oft-discussed "existential anti-pattern," but notice that
unlike e.g. `data CRC32Data = forall a. CRC32Data a => CRC32Data a`, we **never
lose or hide type information** and always operate over a well-defined
structure.

You could write something analogous with `HList`, but to me it seems easier to
understand the code when it's all based around one central type, instead of each
operation specifying the necessary constraints on the `HList`.

# Future work

My conclusion so far is "don't panic; find analogies to the standard library and
you can get pretty far just by turning the crank."

Ideally these classes could be defined without referring to `f` all the way
through, but then you deal with even more `newtype`-wrapping and unwrapping
everywhere.

I would like to understand more about what's going on here, so if you
know more about any of these topics and/or are interested in what I
just showed, please come chat with me (or hit up `peddie` on `#bfpg`).
