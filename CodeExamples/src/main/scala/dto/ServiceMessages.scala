package dto

import dto.ItemType.ItemType

import java.time.LocalDate

object ServiceMessages {

  trait DTO

  /** Задолженность за месяц
    * @param seq порядковый номер задолженности, начиная с 1 для самого первого месяца
    * @param number номер контракта
    * @param date дата публикации, для начального сальдо - дата учёта начального сальдо (balanceBegin.getBalanceDate)
    * @param billingMonth месяц учёта. Первое число
    * @param overdueDate дата, с которой начисление считается задолженностью
    * @param amount начислено за месяц. Может быть отрицательным в случае корректировок (например годовая корректировка за отопление)
    * @param negateBills отрицательные начисления, распределённые по начислениям других месяцев
    * @param payments платежи/кусок платежа, оплачивающий начисление этого месяца (дата платежа может быть из последующих месяцев)
    * @param itemType тип, начисление или начальное сальдо
    */
  case class BillItem(
      seq: Int,
      number: String,
      date: LocalDate,
      billingMonth: LocalDate,
      overdueDate: LocalDate,
      amount: BigDecimal,
      negateBills: List[NegateBill] = List.empty,
      payments: List[Payment] = List.empty,
      itemType: ItemType
  ) extends DTO

  /** Отрицательные начисления для распределения по месяцам с обычными начислениями
    * Начиная с предыдущего и до самого старого. Если что останется, распределяется как платежи
    * @param billingMonth когда реально было отрицательное начисление (месяц в который делали корректировку, превысившую положительное начисление)
    * @param amount сумма отрицательного начисления, причитающаяся на конкретный месяц начисления ExtStatementOfAccountItem
    */
  case class NegateBill(
      seq: Int,
      billingMonth: LocalDate,
      amount: BigDecimal
  ) extends DTO

  /** Платежи
    */
  case class Payment(
      seq: Int,
      number: String,
      date: LocalDate,
      amount: BigDecimal,
      originalAmount: BigDecimal,
      paymentId: String
  ) extends DTO

}

/** Тип ExtStatementOfAccountItem
  * Служит для различения айтемов с начислением и айтема с начальным начислением
  * Они могут быть в одном календарном месяце, тогда в первую очередь
  *  распределяем платежи в айтем с начальным начислением (определяется параметром sortingIndex)
  */
object ItemType extends Enumeration {
  type ItemType = Value
  protected case class Val(sortingIndex: Int) extends super.Val
  implicit def valueToItemTypeVal(x: Value): Val = x.asInstanceOf[Val]
  val MANUAL_BILL: Val = Val(0)
  val BILL: Val = Val(1)
}
