## ------------------------------------------------------------------------

library(analysisRecipes)
library(SparkR)

sparkHome <- "/Users/naren/softwares/spark-2.3.1-bin-hadoop2.7/"
sparkMaster <- "local[1]"
sparkPackages <- c("org.apache.spark:spark-sql-kafka-0-10_2.11:2.3.1")

kafkaBootstrapServers <- "172.25.0.144:9092,172.25.0.98:9092,172.25.0.137:9092"
consumerTopic <- "test_naren"
writeStreamCheckpoint <- "/tmp/checkpoints/1"

sparkRSessionCreateIfNotPresent(sparkHome = sparkHome, master = sparkMaster, sparkPackages = sparkPackages)

streamObj <- read.stream(source = "kafka", kafka.bootstrap.servers = kafkaBootstrapServers, subscribe = consumerTopic)
streamObj <- read.stream(source = "kafka", kafka.bootstrap.servers = kafkaBootstrapServers, subscribe = consumerTopic)

schema <- structType(structField("customerId", "string"),
                     structField("groTransaction", "string"),
                     structField("elecTransaction", "string"),
                     structField("timeStamp", "string")
)

printSchema(streamObj)

recipeObj <- readInputSpark(input = streamObj)

recipeObj %>% castKafkaStreamAsString %>% convertKafkaValueFromJson(schema = schema) -> recipeObj
recipeObj@recipe

recipeObj %>% generateOutput() -> recipeObj
streamObj <- recipeObj@output[[2]]
printSchema(streamObj)

write.stream(streamObj, 'console')


