package net.jadedungeon.scalautil.common

import org.slf4j.LoggerFactory
import org.slf4j.Logger

trait EnvPropsComponent extends Logging {
	import java.util.Properties

	val envProps: Properties

	def getProperty(key: String) = envProps.getProperty(key)
}
