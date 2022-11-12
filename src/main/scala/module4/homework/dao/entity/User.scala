package module4.homework.dao.entity

case class Role(
    code: RoleCode,
    name: String
)

case class User(
    id: UserId,
    firstName: String,
    lastName: String,
    age: Int
)

case class UserToRole(roleId: RoleCode, userId: UserId)

case class RoleCode(code: String) extends AnyVal
case class UserId(id: String) extends AnyVal