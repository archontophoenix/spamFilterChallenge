/** Marks something you should complete to finish this challenge. */
object TODO {
  def apply (msg: String): Nothing =
    throw new AssertionError("Not finished: " + msg)
}

/** Utility for extracting words. */
object Words {
  /** Whether a character is whitespace, for the purposes of this challenge. */
  def isWhite (ch: Char) = Character.isWhitespace(ch)
  /** Converts a character sequence to a stream of words. */
  def apply (chars: Seq[Char]): Stream[String] = TODO("Words.apply")
}

/**
 * A group of training examples. Messages are delimited by newlines, words by
 * whitespace. Empty messages (those with no words) are not counted.
 *
 * Extra credit!: In this solution, an `Examples` holds on to the `messages`
 * stream it is given (a case class's parameters are effectively `public`, so
 * Scala is not free to discard them once it finishes constructing the object
 * to which they are passed). However, the original stream is not needed once
 * an `Examples` has extracted the required information from it. How can you
 * change this class so that unneeded values are recognized as garbage?
 */
case class Examples (messages: Stream[Char]) {
  /** How many messages in `messages`. */
  val messageCount: Int = TODO("messageCount")
  /** How many words (including duplicates) in `messages`. */
  val wordCount: Int = TODO("wordCount")
  /** All the words in `messages`. */
  lazy val dictionary: Set[String] = TODO("dictionary")
  /** The occurrence count for a word in `messages` (zero if not present). */
  def occurrences (word: String): Int = TODO("occurrences")
}

/**
 * A filter trained by examples of spam and ham (where &ldquo;ham&rdquo; means
 * &ldquo;not spam&rdquo;).
 */
case class Filter (spam: Examples, ham: Examples) {
  /** Laplacian smoother. */
  val laplaceSmoother = 1.0
  /** Total message count, adjusted by Laplacian smoothing. */
  private val smoothedMessageCount = 
    spam.messageCount + ham.messageCount + laplaceSmoother * 2
  /** Probability that a message is spam, without looking at the message. */
  private val spamProb =
    (spam.messageCount + laplaceSmoother) / smoothedMessageCount
  /** Probability that a message is ham, without looking at the message. */
  private val hamProb =
    (ham.messageCount + laplaceSmoother) / smoothedMessageCount
  /** Size of overall dictionary (of both spam and ham). */
  private val dictSize = (spam.dictionary ++ ham.dictionary).size
  /** The probability that `word` is a message in class `x`. */
  private def pGiven (word: String, x: Examples): Double =
    (x.occurrences(word) + laplaceSmoother) /
      (x.wordCount + laplaceSmoother * dictSize)
  /**
   * The probability that the given message is spam, as a number between zero
   * and one.
   */
  def isSpam (message: Seq[Char]): Double = {
    val words = Words(message)
    val pSpam = words.foldLeft(spamProb)(_ * pGiven(_,spam))
    val pHam = words.foldLeft(hamProb)(_ * pGiven(_,ham))
    pSpam / (pSpam + pHam)
  }
  override def toString: String =
    "spamProb = %3.2f%%   hamProb = %3.2f%%   dictSize = %d".format(
      spamProb * 100.0,hamProb * 100.0,dictSize)
}

/**
 * Program that prompts for spam and ham training files, then classifies
 * messages you enter.
 */
object Spam extends App {
  def promptAndRead (prompt: String): String = {
    print(prompt)
    readLine
  }
  import scala.io.BufferedSource
  import java.io.FileInputStream
  def withFile [X] (prompt: String, whatToDo: BufferedSource => X): X = {
    val bs =
      new BufferedSource(new FileInputStream(promptAndRead(prompt + ": ")))
    try
      whatToDo(bs)
    catch {
      case rx: RuntimeException =>
        bs.close
        throw rx
      case x =>
        throw x
    }
  }
  val filter =
    Filter(
      Examples(withFile("Spam example file",_.toStream)),
      Examples(withFile("Ham example file",_.toStream)))
  println("\nResulting filter: " + filter)
  println
  def run (msg: String): Unit =
    if (msg.isEmpty) {
      val firstLine = promptAndRead("Message to classify (newline to end):\n")
      if (! firstLine.isEmpty) run(firstLine)
    } else {
      val nextLine = readLine
      if (nextLine.isEmpty) {
        printf("Spam likelihood: %3.2f%%\n\n",100.0 * filter.isSpam(msg))
        run("")
      } else
        run(msg + " " + nextLine)
    }
  run("")
}
