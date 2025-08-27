import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { User, Post, Comment, UserFilterInput, UserSortInput, UserConnection, UserQueryArgs } from '../../schema'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, COMMENT_INCLUDES } from '../../schema-helpers'

@Resolver(() => User)
export class UserResolver {
	@Query(() => UserConnection)
	async users(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | undefined,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		const args: UserQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildUserConnectionConfig(args, info)

		const items = await prisma.user.findMany(config.findManyOptions)
		const totalCount = await prisma.user.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as UserConnection
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		return await prisma.user.findUnique({ where: { id } }) as User | null
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		return await prisma.user.create({ data: { name, email, bio } }) as User
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		return await prisma.post.findMany({
			where: { authorId: user.id },
			include: POST_INCLUDES,
		}) as Post[]
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { authorId: user.id },
			include: COMMENT_INCLUDES,
		}) as Comment[]
	}
}
