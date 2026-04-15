<!-- last-updated: 2026-04-15 -->
# Brag Doc

## 2026

### Q2
OKR context: (Q2 OKRs pending fetch, document ID: 1JVn-DbNzqNGTX0qVuLVET65yqOeyesiMybLAV07rsk8)

Elected to the Substrait PMC, reflecting sustained upstream leadership across the substrait-io ecosystem. Continued driving Trino function migration and Substrait correctness improvements, while actively reviewing contributions across substrait-go, substrait-java, substrait-validator, and substrait (spec).

#### Apr

Became a Substrait PMC member, recognizing contributions to the project's governance and technical direction. Drove multiple upstream improvements (enum arg support in substrait-go, test correctness in substrait spec), landed Trino function migrations internally, and maintained high review velocity across the substrait-io ecosystem.

##### W1 (Mar 31 to Apr 6)

- Migrated pgcompatible array functions to DDSQL/Substrait/DataFusion ([DataDog/dd-source#394357](https://github.com/DataDog/dd-source/pull/394357))
- Added Trino `json_format` function support ([DataDog/dd-source#392480](https://github.com/DataDog/dd-source/pull/392480))
- Dropped unused protos from ddsqlizer_legacy.proto ([DataDog/dd-source#399732](https://github.com/DataDog/dd-source/pull/399732), [DataDog/dd-go#229910](https://github.com/DataDog/dd-go/pull/229910))
- Fixed JSON UDT literal decoding in Substrait consumer ([DataDog/dd-source#401215](https://github.com/DataDog/dd-source/pull/401215))
- Switched substrait spec test extension references to URN format ([substrait-io/substrait#1028](https://github.com/substrait-io/substrait/pull/1028))
- Reviewed: LibDataFusion Unnest Rel ([DataDog/dd-source#392755](https://github.com/DataDog/dd-source/pull/392755)), array_sort fix ([DataDog/dd-source#395919](https://github.com/DataDog/dd-source/pull/395919)), named_struct ([DataDog/dd-source#399423](https://github.com/DataDog/dd-source/pull/399423)), substrait-executor dialect update ([DataDog/dd-source#398397](https://github.com/DataDog/dd-source/pull/398397)), pixi build env for substrait spec ([substrait-io/substrait#1021](https://github.com/substrait-io/substrait/pull/1021)), plan execution behavior in substrait-java ([substrait-io/substrait-java#791](https://github.com/substrait-io/substrait-java/pull/791))

##### W2 (Apr 7 to Apr 13)

- Became a Substrait PMC member (Apr 7)
- Fixed return type for pgcompatible_array_subscript_operator ([DataDog/dd-source#403286](https://github.com/DataDog/dd-source/pull/403286))
- Added parse_datetime/format_datetime shadow variant ([DataDog/dd-source#407067](https://github.com/DataDog/dd-source/pull/407067))
- Updated substrait-go parser to Substrait v0.87.0 with enum arg support ([substrait-io/substrait-go#222](https://github.com/substrait-io/substrait-go/pull/222))
- Fixed decimal literal type normalization in substrait-go test cases ([substrait-io/substrait-go#221](https://github.com/substrait-io/substrait-go/pull/221))
- Fixed std_dev and variance test compact table row format in substrait spec ([substrait-io/substrait#1043](https://github.com/substrait-io/substrait/pull/1043))
- Exposed GenerateBehaviorTestsFromFiles as public API in substrait-conformance ([DataDog/substrait-conformance#80](https://github.com/DataDog/substrait-conformance/pull/80))
- Bumped substrait-go in apache/arrow-go ([apache/arrow-go#754](https://github.com/apache/arrow-go/pull/754))
- Reviewed: Cast expression in substrait-explain ([DataDog/substrait-explain#83](https://github.com/DataDog/substrait-explain/pull/83)), supported libraries + breaking change policy doc ([substrait-io/substrait#1026](https://github.com/substrait-io/substrait/pull/1026)), userDefined type variation in substrait-java ([substrait-io/substrait-java#794](https://github.com/substrait-io/substrait-java/pull/794)), CAST nullability fix ([DataDog/dd-source#405313](https://github.com/DataDog/dd-source/pull/405313)), ws_cast/ws_assert UDFs ([DataDog/dd-source#405083](https://github.com/DataDog/dd-source/pull/405083), [DataDog/dd-source#405085](https://github.com/DataDog/dd-source/pull/405085)), substrait-java v0.87.0 bump ([substrait-io/substrait-java#797](https://github.com/substrait-io/substrait-java/pull/797)), substrait-java CI fix ([substrait-io/substrait-java#813](https://github.com/substrait-io/substrait-java/pull/813)), substrait-java builder API ([substrait-io/substrait-java#773](https://github.com/substrait-io/substrait-java/pull/773)), substrait spec test bug fix ([substrait-io/substrait#1038](https://github.com/substrait-io/substrait/pull/1038)), slice() UDF ([DataDog/dd-source#407869](https://github.com/DataDog/dd-source/pull/407869))

##### W3 (Apr 14 to Apr 20)

- Added ExtensionTable method to substrait-go Builder interface ([substrait-io/substrait-go#228](https://github.com/substrait-io/substrait-go/pull/228))
- Investigating expressions in table functions (QSEM-438), which account for ~1M failing queries in the last 2 weeks ([analysis notebook](https://app.datadoghq.com/notebook/14291133))
- Reviewed: strict type-parameter checking in substrait spec ([substrait-io/substrait#1048](https://github.com/substrait-io/substrait/pull/1048)), substrait-go dependency updates ([substrait-io/substrait-go#229](https://github.com/substrait-io/substrait-go/pull/229), [substrait-io/substrait-go#230](https://github.com/substrait-io/substrait-go/pull/230), [substrait-io/substrait-go#231](https://github.com/substrait-io/substrait-go/pull/231)), substrait-validator antlr migration ([substrait-io/substrait-validator#501](https://github.com/substrait-io/substrait-validator/pull/501)), substrait-java scalatest bump ([substrait-io/substrait-java#815](https://github.com/substrait-io/substrait-java/pull/815))

### Q1

#### Mar

##### W4 (Mar 24 to Mar 30)

- Migrated pgcompatible array functions to DDSQL/Substrait/DataFusion ([DataDog/dd-source#394357](https://github.com/DataDog/dd-source/pull/394357))
- Added Trino `json_format` function support ([DataDog/dd-source#392480](https://github.com/DataDog/dd-source/pull/392480))
- Reviewed: pixi build environment for substrait spec ([substrait-io/substrait#1021](https://github.com/substrait-io/substrait/pull/1021)), HMSResolver endpoint ([DataDog/dd-source#394606](https://github.com/DataDog/dd-source/pull/394606))
