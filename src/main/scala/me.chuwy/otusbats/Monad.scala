package me.chuwy.otusbats

import scala.collection.immutable.HashSet

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] =  flatMap(fa)(identity)

}

object Monad {

  implicit val monadOption: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = flatMap(fa)(a => point(f(a)))
  }

  implicit val monadList: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = flatMap(fa)(a => point(f(a)))
  }

  implicit val hashMap: Monad[HashSet] = new Monad[HashSet] {
    override def flatMap[A, B](fa: HashSet[A])(f: A => HashSet[B]): HashSet[B] = fa.flatMap(f)

    override def point[A](a: A): HashSet[A] = HashSet(a)

    override def map[A, B](fa: HashSet[A])(f: A => B): HashSet[B] = fa.map(f)
  }

}
