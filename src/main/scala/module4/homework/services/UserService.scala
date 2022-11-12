package module4.homework.services

import module4.homework.dao.entity.{Role, RoleCode, User}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db
import zio.macros.accessible
import zio.{Has, RIO, ZIO, ZLayer}

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.list()
            userDtoList <- enhanceUserWithRoles(users)
        } yield userDtoList
        
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
            role <- userRepo.findRoleByCode(roleCode)
              .mapError(new Throwable(_))
              .someOrFailException
            user <- userRepo.createUser(user)
            _ <- userRepo.insertRoleToUser(roleCode, user.id)
        } yield UserDTO(user, Set(role))
        
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.listUsersWithRole(roleCode)
            userDtoList <- enhanceUserWithRoles(users)
        } yield userDtoList

        private def enhanceUserWithRoles(users: List[User]): RIO[db.DataSource, List[UserDTO]] =
            ZIO.foreach(users)(user => userRepo.userRoles(user.id)
              .map(roles => UserDTO(user, roles.toSet)))
        
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] =
        ZLayer.fromService(new Impl(_))
}

case class UserDTO(user: User, roles: Set[Role])