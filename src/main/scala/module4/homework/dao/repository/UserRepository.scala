package module4.homework.dao.repository

import io.getquill.context.ZioJdbc._
import module4.homework.dao.entity._
import module4.phoneBook.db
import zio.{Has, ULayer, ZLayer}


object UserRepository{


    private val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[User]
        def createUsers(users: List[User]): QIO[List[User]]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{

        private lazy val userSchema = quote {
            querySchema[User](""""User"""")
        }


        private lazy val roleSchema = quote {
            querySchema[Role]("""Role""")
        }

        private lazy val userToRoleSchema = quote {
            querySchema[UserToRole]("""UserToRole""")
        }

        def findUser(userId: UserId): Result[Option[User]] =
            dc.run(userSchema.filter(_.id == lift(userId.id)).take(1))
          .map(_.headOption)
        
        def createUser(user: User): Result[User] =
            dc.run(userSchema.insert(lift(user)))
              .as(user)

        /** <a href="https://getquill.io/#writing-queries-actions-batch-insert">Doc</a> */
        def createUsers(users: List[User]): Result[List[User]] =
            dc.run(liftQuery(users).foreach(
                v => userSchema.insert(User(v.id, v.firstName, v.lastName, v.age))
            ))
        
        def updateUser(user: User): Result[Unit] =
            dc.run(userSchema.filter(_.id == user.id)
                          .update(lift(user))).unit
        
        def deleteUser(user: User): Result[Unit] =
            dc.run(userSchema.filter(_.id == user.id)
                          .delete).unit
        
        def findByLastName(lastName: String): Result[List[User]] =
            dc.run(userSchema.filter(_.lastName contains lastName))
        
        def list(): Result[List[User]] =
            dc.run(userSchema)
        
        def userRoles(userId: UserId): Result[List[Role]] =
            dc.run(
                for {
                    // SELECT * FROM Role AS r LEFT JOIN UserToRole AS utr ON utr.userId = ?0 WHERE utr.roleId = utr.roleId
                    userToRoles <- userToRoleSchema.filter(_.userId == userId)
                    role <- roleSchema.filter(_.typedId == userToRoles.roleId)
                } yield role
            )
        
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] =
            dc.run(
                for {
                  u <- userSchema.filter(_.typedId == userId)
                  r <- roleSchema.filter(_.typedId == roleCode)
                } yield userToRoleSchema.insert(UserToRole(r.typedId, u.typedId))
            ).unit
        
        def listUsersWithRole(roleCode: RoleCode): Result[List[User]] =
            dc.run(
                for {
                    userId <- userToRoleSchema.filter(_.roleId == roleCode)
                                              .map(_.userId)
                    users <- userSchema.filter(_.typedId == userId)
                } yield users
            )
        
        def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] =
            dc.run(roleSchema.filter(_.typedId == roleCode).take(1))
              .map(_.headOption)
                
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}