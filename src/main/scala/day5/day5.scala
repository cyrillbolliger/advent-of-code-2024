package me.cyrill.aoc2024.day5

import scala.io.Source

val inputPath = "src/main/scala/day5/input.txt"
lazy val input = Source.fromFile(inputPath).getLines().toArray

// Naming: Job = pages to produce

type Job = List[Int]
type Rules = Map[Int, Set[Int]]

def getInput(in: Array[String]): (Array[String], Array[String]) =
  val separator = in.indexOf("")
  (in.take(separator), in.drop(separator + 1))

def parseRules(in: Array[String]): Rules =
  in.toSet
    .map(r => r.split("\\|").map(_.toInt))
    .groupMap(_(0))(_(1))

def parseJobs(in: Array[String]): Seq[Job] =
  in.map(s => s.split(",").map(_.toInt).toList).toSeq

def doesntContain[T](l: List[T], except: Set[T]): Boolean =
  l.forall(!except.contains(_))

def checkJob(job: Job, rules: Rules): Boolean =
  job.zipWithIndex.forall((p, idx) =>
    val before = job.take(idx)
    val rule = rules.getOrElse(p, Set.empty)
    doesntContain(before, rule)
  )

def getMiddlePageNum(job: Job): Int =
  job(job.length / 2)

def solve1: Int =
  val (r, p) = getInput(input)
  val rules = parseRules(r)
  val jobs = parseJobs(p)

  jobs.filter(checkJob(_, rules)).map(getMiddlePageNum).sum
