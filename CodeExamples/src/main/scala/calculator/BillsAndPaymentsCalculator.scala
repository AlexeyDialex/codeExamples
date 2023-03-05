package calculator

import dto.ServiceMessages.{BillItem, NegateBill, Payment}

import java.time.LocalDate
import scala.annotation.tailrec

object BillsAndPaymentsCalculator {

  /** При "квитировании" компенсирующие операции применяются в порядке:
    * Списания -> Отрицательные (начисления+списания) на предыдущие месяцы -> Отрицательные платежи ->
    * -> Оставшиеся отрицательные (начисления+списания) -> Платежи -> Оставшиеся платежи
    *
    * Цель квитирования - возможность взять детализацию задолженности за любой период (интервал месяцев),
    *  (отрезать любой кусок сформированной "колбасы" из айтемов)
    *  и получить корректную сумму задолженности за этот период,
    *  без необходимости смотреть на начисления/платежи/списания других месяцев, не вошедших в рассматриваемый период
    *
    * Списания
    *  - это корректировка начисления, у них есть период.
    *  Равномерно размазываем сумму списания на начисления его периода, и положительные и отрицательные
    *  При этом могут появиться отрицательные начисления+списания, которые надо обрабатывать как отрицательные начисления
    *
    * Отрицательные начисления (+списания)
    *  используются для погашения предыдущих начислений, начиная с предыдущего, и далее до самого старого.
    *  Если в самом старом месяце ещё остались отрицательные начисления, распределяем их тем же образом что платежи
    *
    * Платежи
    *  Сторно и сторнированные платежи ("кто" и "кого" сторнирует) выкидываем и не учитываем.
    *  - отрицательные используются для предварительного погашения положительных платежей, аналогично отрицательным начислениям,
    *   начиная с ближайшего предыдущего, и далее до самого старого.
    *   Если отрицательный платеж гасит все предыдущие платежи, остаток оставляем в первом месяце, и там он работает как начисление
    *   (возможно лучше помещать его в месяц, когда был этот отрицательный платеж?)   *  - положительные используются для погашения положительных начислений,
    *  начиная со самого старого непогашенного
    *
    *  Если останутся лишние платежи, добавить их в последний месяц
    */

  def calculatingBillItems(
      rawBills: List[BillItem],
      paymentItems: List[Payment]
  ): List[BillItem] = {
    val (positiveBillItems, negateBills) = extractNegateBills(
      rawBills
    )
    val (negatePaymentItems, positivePaymentItems) =
      paymentItems.partition(_.amount < 0)

    val (billItemsWithNegateBills, remainingNegateBills) =
      distributeNegateBills(positiveBillItems, negateBills)
    val (remainingPositivePayments, remainingNegatePaymentItems) =
      distributeNegatePayments(positivePaymentItems, negatePaymentItems)

    //нераспределённые отрицательные платежи кладём в первый месяц
    //по идее должно корректно суммироваться при последующем распределении отрицательных начислений и положительных платежей
    val billItemsWithRemainingNegatePayments =
      addToFirstBillItem(billItemsWithNegateBills, remainingNegatePaymentItems)

    //отрицательные начисления, не распределённые по предыдущим начислениям, распределяем аналогично положительным платежам
    //undistributedNegateBills - то что останется после распределения отрицательных начислений по всем начислениям
    val (billItemsWithRemainingNegateBills, undistributedNegateBills) =
      distributeNegateBillItems(
        billItemsWithRemainingNegatePayments,
        remainingNegateBills
      )

    val withAllNegateBills = addNegateToLastBillItem(
      billItemsWithRemainingNegateBills,
      undistributedNegateBills
    )

    val (billItemsWithPayments, undistributedPayments) =
      distributePaymentItems(withAllNegateBills, remainingPositivePayments)
    //нераспределённые (не потраченные) положительные платежи кладём в последний месяц
    val resultItems =
      addToLastBillItem(billItemsWithPayments, undistributedPayments)
    resultItems
  }

  val billItemsSorting: BillItem => (LocalDate, Int) =
    (item: BillItem) => (item.billingMonth, item.itemType.sortingIndex)

  private def addToLastBillItem(
      billItems: List[BillItem],
      remainPayments: List[Payment]
  ): List[BillItem] = {
    billItems match {
      case Nil          => Nil
      case init :+ last => init :+ last.withPaymentItems(remainPayments: _*)
    }
  }

  private def addNegateToLastBillItem(
      billItems: List[BillItem],
      negateBills: List[NegateBill]
  ): List[BillItem] = {
    billItems match {
      case Nil          => Nil
      case init :+ last => init :+ last.withNegateBills(negateBills: _*)
    }
  }

  private def addToFirstBillItem(
      billItems: List[BillItem],
      negatePaymentItems: List[Payment]
  ): List[BillItem] = {
    billItems match {
      case Nil          => Nil
      case head :: tail => head.withPaymentItems(negatePaymentItems: _*) :: tail
    }
  }

  private def distributeNegateBillItems(
      positiveBillItems: List[BillItem],
      negateBills: List[NegateBill]
  ): (
      List[BillItem],
      List[NegateBill]
  ) = {
    positiveBillItems
      .foldLeft((List.empty[BillItem], negateBills)) {
        case ((resultBillItems, negateBillItems), currentBillItem) =>
          val (currentBillItemWithNegateBills, remainNegateBills) =
            distributeNegateBills(currentBillItem, negateBillItems)
          (resultBillItems :+ currentBillItemWithNegateBills, remainNegateBills)
      }
  }

  /** @param billItem месяц с начислением
    * @param negateBillsForDistribute неотрицательные платежи
    * @return новый billItem и оставшиеся платежи
    */
  @tailrec
  final def distributeNegateBills(
      billItem: BillItem,
      negateBillsForDistribute: List[NegateBill]
  ): (BillItem, List[NegateBill]) = {

    negateBillsForDistribute match {
      case negateBills if billItem.resultAmount == 0 =>
        (billItem, negateBills)

      case Nil =>
        (billItem, Nil)

      case currentNegateBill :: remainingNegateBills =>
        val amountForDistribute = currentNegateBill.amount.min(
          billItem.resultAmount
        ) //сумма, которая будет распределена на этот месяц
        val distributedPart =
          currentNegateBill.copy(amount = amountForDistribute)
        val undistributedPart = currentNegateBill.copy(amount =
          currentNegateBill.amount - amountForDistribute
        )
        val nonZeroUndistributedPart = Option(
          undistributedPart
        ) //если распределился всё отрицательное начисление, откидываем его
          .filterNot(_.amount == 0)
        val newBillItem = billItem.withNegateBills(distributedPart)
        distributeNegateBills(
          newBillItem,
          nonZeroUndistributedPart.toList ++ remainingNegateBills
        )
    }
  }
  private def distributePaymentItems(
      positiveBillItems: Iterable[BillItem],
      paymentItemsForDistribute: List[Payment]
  ): (List[BillItem], List[Payment]) = {
    positiveBillItems
      .foldLeft(
        (List.empty[BillItem], paymentItemsForDistribute)
      ) { case ((billItems, paymentItems), currentBillItem) =>
        val (billItemWithPayments, remainPayments) =
          distributingPositivePayments(currentBillItem, paymentItems)
        (billItems :+ billItemWithPayments, remainPayments)
      }
  }

  /** @param billItem месяц с начислением
    * @param paymentsForDistribute неотрицательные платежи
    * @return новый billItem и оставшиеся платежи
    */
  @tailrec
  final def distributingPositivePayments(
      billItem: BillItem,
      paymentsForDistribute: List[Payment]
  ): (BillItem, List[Payment]) = {
    paymentsForDistribute match {
      case payments if billItem.resultAmount == 0 =>
        (billItem, payments)

      case Nil =>
        (billItem, Nil)

      case currentPayment :: remainingPayments =>
        val amountForDistribute = currentPayment.amount.min(
          billItem.resultAmount
        ) //сумма, которая будет распределена на этот месяц
        val distributedPart = currentPayment.copy(amount = amountForDistribute)
        val undistributedPart = currentPayment.copy(amount =
          currentPayment.amount - amountForDistribute
        )
        val nonZeroUndistributedPart = Option(
          undistributedPart
        ) //если распределился весь отрицательный платеж, откидываем его
          .filterNot(_.amount == 0)
        val newBillItem = billItem.withPaymentItems(distributedPart)
        distributingPositivePayments(
          newBillItem,
          nonZeroUndistributedPart.toList ++ remainingPayments
        )
    }
  }

  /** Выделяет отрицательные суммы начисление+списания в отдельный [[NegateBill]] <p>
    *  item с отрицательным начислением+списания не убираем (нам всё ещё нужен этот месяц),
    *  а корректируем item.amount чтобы получить нулевое начисление+списания
    *
    * @param billItems начисления с распределёнными по ним списаниями
    * @return (неотрицательные начисления, отрицательные начисления)
    */
  private def extractNegateBills(
      billItems: List[BillItem]
  ): (List[BillItem], List[NegateBill]) = {
    val positiveBillsAndNegate = billItems.map { billItem =>
      val negateDiff = billItem.resultAmount.min(BigDecimal(0))
      val newBillItem = billItem.copy(amount = billItem.amount - negateDiff)
      val maybeNegateBill = Option(
        NegateBill(
          0,
          billItem.billingMonth,
          -negateDiff
        )
      )
        .filter(_.amount > 0)
      (newBillItem, maybeNegateBill)
    }
    val (positiveBillItems, maybeNegateBills) = positiveBillsAndNegate.unzip
    (positiveBillItems, maybeNegateBills.flatten)
  }

  /** Распределяем отрицательные начисления (+списания)
    *  происходит погашение предыдущих начислений, начиная с предыдущего, и далее до самого старого.
    *  Если после распределения в самый старый месяц ещё остались отрицательные начисления,
    *  оставляем их для дальнейшего распределения в качестве NegateBill
    * @param positiveBills неотрицательные начисления+списания
    * @param negateBills отрицательные начисления
    */
  def distributeNegateBills(
      positiveBills: List[BillItem],
      negateBills: List[NegateBill]
  ): (List[BillItem], List[NegateBill]) = {
    negateBills
      .foldLeft(
        (positiveBills, List.empty[NegateBill])
      ) { case ((billItems, remainingNegateBills), negateBill) =>
        val (newBills, maybeRemainingNegateBill) =
          distributeNegateBill(billItems, negateBill)
        (newBills, remainingNegateBills ++ maybeRemainingNegateBill)
      }
  }

  /** Распределяем отрицательное начисление по предыдущим положительным начислениям <p>
    * Может быть остаток
    */
  private def distributeNegateBill(
      billItems: List[BillItem],
      negateBill: NegateBill
  ): (List[BillItem], Option[NegateBill]) = {
    val (billsBefore, billsAfter) =
      billItems.partition(_.billingMonth.isBefore(negateBill.billingMonth))
    val (billItemsWithNegateBills, maybeRemainingNegateBill) = billsBefore
      .foldRight((List.empty[BillItem], Option(negateBill))) {
        case (currentBill, (accBillItems, Some(undistributedNegateBill)))
            if currentBill.resultAmount > 0 =>
          val amountForDistribute =
            undistributedNegateBill.amount.min(currentBill.resultAmount)
          val currentNegateBill =
            undistributedNegateBill.copy(amount = amountForDistribute)
          val maybeRemainingNegateBill = Option(
            undistributedNegateBill.copy(amount =
              undistributedNegateBill.amount - amountForDistribute
            )
          )
            .filter(_.amount > 0)
          val billWithNegateBill =
            currentBill.withNegateBills(currentNegateBill)
          (billWithNegateBill :: accBillItems, maybeRemainingNegateBill)

        case (currentBill, (accBillItems, currentNegateBill)) =>
          (currentBill :: accBillItems, currentNegateBill)
      }
    (billItemsWithNegateBills ::: billsAfter, maybeRemainingNegateBill)
  }

  /** гасим отрицательные платежи, аналогично отрицательным начислениям,
    *   начиная с ближайшего предыдущего платежа, и далее до самого старого.
    *   Если отрицательный платеж гасит все предыдущие платежи, остаток оставляем в месяце billingMonth платежа
    * @param paymentItems не отрицательные платежи
    * @param negatePaymentItems отрицательные платежи
    * @return (оставшиеся непогашенные отрицательные и не отрицательные платежи) - один из этих списков будет пустым
    */
  def distributeNegatePayments(
      paymentItems: List[Payment],
      negatePaymentItems: List[Payment]
  ): (List[Payment], List[Payment]) = {
    negatePaymentItems
      .foldLeft(
        (paymentItems, List.empty[Payment])
      ) { case ((billItems, remainingNegateBills), negateBill) =>
        val (newPayments, maybeRemainingNegatePayments) =
          distributeNegatePayment(billItems, negateBill)
        (newPayments, remainingNegateBills ++ maybeRemainingNegatePayments)
      }
  }

  /** Отрицательный платеж гасит положительные платежи, начиная с предыдущего и вплоть до самого старого
    * Может быть остаток отрицательного платежа, если не хватило предыдущих положительных платежей
    * @return (оставшиеся положительные платежи, оставшиеся отрицательные платежи)
    */
  private def distributeNegatePayment(
      positivePayments: List[Payment],
      negatePayment: Payment
  ): (List[Payment], Option[Payment]) = {
    val (paymentsBefore, paymentsAfter) =
      positivePayments.partition(_.date.isBefore(negatePayment.date))
    val (newPaymentsBefore, maybeRemainingNegatePayment) = paymentsBefore
      .foldRight((List.empty[Payment], Option(negatePayment))) {
        case (
              currentPositivePayment,
              (accPaymentItems, Some(remainingNegatePayment))
            ) =>
          val amountForDistribute =
            remainingNegatePayment.amount.min(currentPositivePayment.amount)
          val maybeRemainingNegatePayment = Option(
            remainingNegatePayment.copy(amount =
              remainingNegatePayment.amount - amountForDistribute
            )
          )
            .filter(_.amount > 0)
          val maybeRemainingPositivePayment = Option(
            currentPositivePayment.copy(amount =
              currentPositivePayment.amount - amountForDistribute
            )
          )
            .filter(_.amount > 0)
          (
            maybeRemainingPositivePayment.toList ::: accPaymentItems,
            maybeRemainingNegatePayment
          )

        case (prevPayment, (accPaymentItems, None)) =>
          (prevPayment :: accPaymentItems, None)
      }
    (newPaymentsBefore ::: paymentsAfter, maybeRemainingNegatePayment)
  }

  /** методы для формирования детализации, в другом серсисе не требуются
    */
  implicit class ExtStatementOfAccountItemCalculating(item: BillItem) {
    def resultAmount: BigDecimal = {
      val paymentsAmount = Option(item.payments)
        .map { _.map(_.amount).sum }
        .getOrElse(BigDecimal(0))
      val negateBillsAmount = item.negateBills
        .map(_.amount)
        .sum
      item.amount - paymentsAmount - negateBillsAmount
    }

    def withNegateBills(negateBillsForAdd: NegateBill*): BillItem = {
      val currentSeq = item.negateBills.length
      val newNegateBills = negateBillsForAdd.toList
        .sortBy(_.billingMonth)
        .zipWithIndex
        .map { case (negateBill, index) =>
          negateBill.copy(seq = currentSeq + index)
        }
      item.copy(negateBills = item.negateBills ++ newNegateBills)
    }

    def withPaymentItems(paymentItemsForAdd: Payment*): BillItem = {
      val currentSeq = item.payments.length
      val newPaymentItems = paymentItemsForAdd.toList
        .sortBy(_.date)
        .zipWithIndex
        .map { case (paymentItem, index) =>
          paymentItem.copy(seq = currentSeq + index)
        }
      item.copy(payments = item.payments ++ newPaymentItems)
    }
  }

  /** Начисления после этой даты ещё не считаются задолженностью<br>
    * Например, если chargeDay = 11, и сегодня 5 марта, метод вернёт дату 25 января. При этом
    *  январские начисления будут считаться задолженностью, а февральские ещё нет
    */
  private[calculator] def debtUnder(chargeDay: Int): LocalDate = {
    val today = LocalDate.now()
    today.minusDays(chargeDay - 1).withDayOfMonth(1)
  }
}
