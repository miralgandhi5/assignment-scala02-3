package edu.knoldus

import org.apache.log4j.Logger

object Application extends App{

  val log: Logger = Logger.getLogger(this.getClass)
  val inputListOne: List[String] = List("hii","hello","how")
  val inputListTwo: List[String] = List("are","you","?")
  val subSequence: List[String] = List("hello","how")
  log.info(s"${Operations.length[String](inputListOne)}\n")
  log.info(s"${Operations.concatList[String](inputListOne,inputListTwo)}\n")
  log.info(s"${Operations.splitList[String](inputListOne,str => str.length == 3)}\n")
  log.info(s"${Operations.hasSubSequence[String](inputListOne,subSequence)}\n")





}
