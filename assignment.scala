// for removing duplicate
val list = List(1,2,3,4)
def dedupe(list:List[Int]):List[Int] = {
  list.foldLeft(List[Int]()){(acc,ele)=>
    if(!acc.contains(ele))(ele::acc)
    else acc
  }
}

val l = dedupe(list)

println(l)


//for reversing list
val list = List(1,2,3,4)

def reverse(list:List[Int]):List[Int] = {
  list.foldLeft(List[Int]()){(acc,ele)=>
    (ele::acc)
  }
}
reverse(list)

// max using recursion
def max(list : List[Int]) : Int = {
  def max(list:List[Int], acc:Int, index : Int):Int = {
    if(list.size == index) {
      acc
    }
    else {
      if(acc < list(index)) max(list, list(index), index + 1)
      else max(list, acc, index+1)
    }
  }
  max(list, -1, 0)
}

val list = List(1,21,3,4,5,6,7)

max(list)
// finding max foldleft

val list = List(1,21,3,4,5,6,7)
def max(list: List[Int]) = {
  list.foldLeft(-1){(acc,ele)=>
    if(acc < ele) ele
    else acc
  }
}

max(list)


